-- This package contains support functions for standard codec building
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,
-- You can obtain one at http://mozilla.org/MPL/2.0/.
--
-- Copyright (c) 2014-2022, Lars Asplund lars.anders.asplund@gmail.com

library ieee;
use ieee.std_logic_1164.all;
use ieee.math_complex.all;
use ieee.numeric_bit.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

use std.textio.all;


-------------------------------------------------------------------------------
-- Package body
-------------------------------------------------------------------------------
package body codec_v1993_pkg is

  -----------------------------------------------------------------------------
  -- Encode and Decode functions of predefined enumerated types
  -----------------------------------------------------------------------------

  -- Boolean
  function encode_boolean(data : boolean) return code_t is
  begin
    if data then
      return "T";
    else
      return "F";
    end if;
  end function;

  procedure decode_boolean(
    constant code   : in    code_t;
    variable index  : inout code_index_t;
    variable result : out   boolean
  ) is
  begin
    result := code(index) = 'T';
    index  := index + CODE_LENGTH_BOOLEAN;
  end procedure;

  -- Character
  function encode_character(data : character) return code_t is
  begin
    -- String start at index 1 (they are defined with a positive range)
    return (1 => data);
  end function;

  procedure decode_character(
    constant code   : in    code_t;
    variable index  : inout code_index_t;
    variable result : out   character
  ) is
  begin
    result := code(index);
    index  := index + CODE_LENGTH_CHARACTER;
  end procedure;

  -- Bit
  function encode_bit(data : bit) return code_t is
  begin
    if data = '1' then
      return "1";
    else
      return "0";
    end if;
  end function;

  procedure decode_bit(
    constant code   : in    code_t;
    variable index  : inout code_index_t;
    variable result : out   bit
  ) is
  begin
    if code(index) = '1' then
      result := '1';
    else
      result := '0';
    end if;
    index := index + CODE_LENGTH_BIT;
  end procedure;

  -- std_ulogic
  function encode_std_ulogic(data : std_ulogic) return code_t is
  begin
    -- The '2 to 2' is used to select the second character of the string representation
    return std_ulogic'image(data)(2 to 2);
  end function;

  procedure decode_std_ulogic(
    constant code   : in    code_t;
    variable index  : inout code_index_t;
    variable result : out   std_ulogic
  ) is
  begin
    result := std_ulogic'value("'" & code(index) & "'");
    index  := index + CODE_LENGTH_STD_ULOGIC;
  end procedure;

  -- severity_level
  function encode_severity_level(data : severity_level) return code_t is
  begin
    -- String start at index 1 (they are defined with a positive range)
    return (1 => character'val(severity_level'pos(data)));
  end function;

  procedure decode_severity_level(
    constant code   : in    code_t;
    variable index  : inout code_index_t;
    variable result : out   severity_level
  ) is
  begin
    result := severity_level'val(character'pos(code(index)));
    index  := index + CODE_LENGTH_SEVERITY_LEVEL;
  end procedure;

  -- file_open_kind
  function encode_file_open_kind(data : file_open_kind) return code_t is
  begin
    -- String start at index 1 (they are defined with a positive range)
    return (1 => character'val(file_open_kind'pos(data)));
  end function;

  procedure decode_file_open_kind(
    constant code   : in    code_t;
    variable index  : inout code_index_t;
    variable result : out   file_open_kind
  ) is
  begin
    result := file_open_kind'val(character'pos(code(index)));
    index  := index + CODE_LENGTH_FILE_OPEN_KIND;
  end procedure;

  -- file_open_status
  function encode_file_open_status(data : file_open_status) return code_t is
  begin
    -- String start at index 1 (they are defined with a positive range)
    return (1 => character'val(file_open_status'pos(data)));
  end function;

  procedure decode_file_open_status(
    constant code   : in    code_t;
    variable index  : inout code_index_t;
    variable result : out   file_open_status
  ) is
  begin
    result := file_open_status'val(character'pos(code(index)));
    index  := index + CODE_LENGTH_FILE_OPEN_STATUS;
  end procedure;


  -----------------------------------------------------------------------------
  -- Encode and Decode functions of predefined scalar types
  -----------------------------------------------------------------------------

  -- integer
  function encode_integer(data : integer) return code_t is
  begin
    return encode_raw_bit_array(bit_array(ieee.numeric_bit.to_signed(data, SIMULATOR_INTEGER_WIDTH)));
  end function;

  procedure decode_integer(
    constant code   : in    code_t;
    variable index  : inout code_index_t;
    variable result : out   integer
  ) is
  begin
    result := to_integer(ieee.numeric_bit.signed(
      decode_raw_bit_array(code(index to index + CODE_LENGTH_INTEGER - 1), SIMULATOR_INTEGER_WIDTH)
    ));
    index  := index + CODE_LENGTH_INTEGER;
  end procedure;

  -- real
  function encode_real(data : real) return code_t is
    constant is_signed : boolean := data < 0.0;
    variable value : real := abs(data);
    variable exp : integer;
    variable low : integer;
    variable high : integer;
  begin
    if value = 0.0 then
      exp := 0;
    else
      exp := integer(floor(log2(value)));
    end if;
    value := value * 2.0 ** (-exp + 53); -- TODO, pq 53 ?
    high := integer(floor(value * 2.0 ** (-31)));
    low := integer(value - real(high) * 2.0 ** 31);
    return encode_boolean(is_signed) & encode_integer(exp) & encode_integer(low) & encode_integer(high);
  end function;

  procedure decode_real(
    constant code   : in    code_t;
    variable index  : inout code_index_t;
    variable result : out   real
  ) is
    variable is_signed : boolean;
    variable exp, low, high : integer;
    variable ret_val : real;
  begin
    decode_boolean(code, index, is_signed);
    decode_integer(code, index, exp);
    decode_integer(code, index, low);
    decode_integer(code, index, high);

    ret_val := (real(low) + real(high) * 2.0**31) * 2.0 ** (exp - 53); -- TODO Support 64 bits
    if is_signed then
      ret_val := -ret_val;
    end if;
    result := ret_val;
  end procedure;

  -- time
  function encode_time(data : time) return code_t is

    function modulo(t : time; m : natural) return integer is
    begin
      return integer((t - (t/m)*m)/SIMULATOR_RESOLUTION) mod m;
    end function;
    variable ret_val : code_t(1 to CODE_LENGTH_TIME) := (others => NUL);
    variable t       : time;
    variable ascii   : natural;

  begin
    t := data;
    for i in CODE_LENGTH_TIME downto 1 loop
      ascii := modulo(t, CODE_NB_VALUES);
      ret_val(i) := character'val(ascii);
      t := (t - (ascii * SIMULATOR_RESOLUTION))/CODE_NB_VALUES;
    end loop;
    return ret_val;
  end function;

  procedure decode_time(
    constant code   : in    code_t;
    variable index  : inout code_index_t;
    variable result : out   time
  ) is
    constant code_int : code_t(1 to CODE_LENGTH_TIME) := code(index to index + CODE_LENGTH_TIME - 1);
    variable r : time;
    variable b : integer;
  begin
    r := SIMULATOR_RESOLUTION * 0;

    for i in code_int'range loop
      b := character'pos(code_int(i));
      r := r * CODE_NB_VALUES;
      if i = 1 and b >= CODE_NB_VALUES/2 then
        b := b - CODE_NB_VALUES;
      end if;
      r := r + b * SIMULATOR_RESOLUTION;
    end loop;

    index := index + CODE_LENGTH_TIME;
    result := r;
  end procedure;


  -----------------------------------------------------------------------------
  -- Encode and Decode functions of predefined composite types (records)
  -----------------------------------------------------------------------------

  -- complex
  function encode_complex(data : complex) return code_t is
  begin
    return encode_real(data.re) & encode_real(data.im);
  end function;

  procedure decode_complex(
    constant code   : in    code_t;
    variable index  : inout code_index_t;
    variable result : out   complex
  ) is
  begin
    decode_real(code, index, result.re);
    decode_real(code, index, result.im);
  end procedure;

  -- complex_polar
  function encode_complex_polar(data : complex_polar) return code_t is
  begin
    return encode_real(data.mag) & encode_real(data.arg);
  end function;

  procedure decode_complex_polar(
    constant code   : in    code_t;
    variable index  : inout code_index_t;
    variable result : out   complex_polar
  ) is
  begin
    decode_real(code, index, result.mag);
    decode_real(code, index, result.arg);
  end procedure;



  -----------------------------------------------------------------------------
  -- Encode and decode functions for range
  -----------------------------------------------------------------------------

  function encode_range(
    constant range_left   : integer;
    constant range_right  : integer;
    constant is_ascending : boolean
  ) return code_t is
  begin
    return encode_integer(range_left) & encode_integer(range_right) & encode_boolean(is_ascending);
  end function;

  function decode_range(constant code : code_t) return range_t is
    constant RANGE_LEFT : integer := decode_integer(
      code(code'left to code'left + CODE_LENGTH_INTEGER - 1)
    );
    constant RANGE_RIGHT : integer := decode_integer(
      code(code'left + CODE_LENGTH_INTEGER to code'left + CODE_LENGTH_INTEGER*2 - 1)
    );
    constant IS_ASCENDING : boolean := decode_boolean(
      code(code'left + CODE_LENGTH_INTEGER*2 to code'left + CODE_LENGTH_INTEGER*2 + CODE_LENGTH_BOOLEAN - 1)
    );
    constant ret_val_ascending  : range_t(RANGE_LEFT to RANGE_RIGHT) := (others => '0');
    constant ret_val_descending : range_t(RANGE_LEFT downto RANGE_RIGHT) := (others => '0');
  begin
    if IS_ASCENDING then
      return ret_val_ascending;
    else
      return ret_val_descending;
    end if;
  end function;

  -- Null range constant
  constant ENCODED_NULL_RANGE : code_t := encode_range(1, 0, true);


  -----------------------------------------------------------------------------
  -- Functions which gives the number of code_t element to be used to encode the type
  -----------------------------------------------------------------------------

  function code_length_string(length : natural) return natural is
  begin
    if length = 0 then
      return CODE_LENGTH_RANGE_TYPE;
    else
      return CODE_LENGTH_RANGE_TYPE + length;
    end if;
  end function;

  function code_length_raw_bit_array(length : natural) return natural is
  begin
    if length = 0 then
      return 0;
    else
      return ceil_div(length, CODE_LENGTH);
    end if;
  end function;

  function code_length_bit_array(length : natural) return natural is
  begin
    if length = 0 then
      return CODE_LENGTH_RANGE_TYPE;
    else
      return CODE_LENGTH_RANGE_TYPE + code_length_raw_bit_array(length);
    end if;
  end function;

  function code_length_bit_vector(length : natural) return natural is
  begin
    return code_length_bit_array(length);
  end function;

  function code_length_numeric_bit_unsigned(length : natural) return natural is
  begin
    return code_length_bit_array(length);
  end function;

  function code_length_numeric_bit_signed(length : natural) return natural is
  begin
    return code_length_bit_array(length);
  end function;

  constant BITS_LENGTH_STD_ULOGIC : positive := positive(ceil(log2(real(LENGTH_STD_ULOGIC))));
  function code_length_std_ulogic_array(length : natural) return natural is
  begin
    -- std_ulogic_array are encoded into string. The array is divided into
    -- groups of 2 std_ulogic_array.
    -- One std_ulogic can represent LENGTH_STD_ULOGIC=9 value: it needs BITS_LENGTH_STD_ULOGIC=3 bits to store it.
    -- In a character (CODE_LENGTH=8 bits), we can store BITS_LENGTH_STD_ULOGIC/CODE_LENGTH=2 std_ulogic elements.
    return CODE_LENGTH_RANGE_TYPE + ceil_div(length, BITS_LENGTH_STD_ULOGIC/CODE_LENGTH);
  end function;

  function code_length_std_ulogic_vector(length : natural) return natural is
  begin
    return code_length_std_ulogic_array(length);
  end function;

  function code_length_numeric_std_unsigned(length : natural) return natural is
  begin
    return code_length_std_ulogic_array(length);
  end function;

  function code_length_numeric_std_signed(length : natural) return natural is
  begin
    return code_length_std_ulogic_array(length);
  end function;


  -----------------------------------------------------------------------------
  -- Encode and decode functions of predefined composite types (arrays)
  -----------------------------------------------------------------------------

  -- string
  function encode_string(data : string) return code_t is
  begin
    -- Modelsim sets data'right to 0 which is out of the positive index
    -- range used by strings.
    if data'length = 0 then
      -- return encode_range(data'left, data'right, data'ascending); -- TBC Why not NULL range ?
      return ENCODED_NULL_RANGE;
    else
      return encode_range(data'left, data'right, data'ascending) & data;
    end if;
  end function;

  procedure decode_string(
    constant code   : in    code_t;
    variable index  : inout code_index_t;
    variable result : out   string
  ) is
  begin
    index := index + CODE_LENGTH_RANGE_TYPE;
    result := code(index to index + code_length_string(result'length) - 1);
    index  := index + code_length_string(result'length);
  end procedure;

  -- raw_bit_array
  function encode_raw_bit_array(data : bit_array) return code_t is
    variable ret_val : code_t(1 to code_length_raw_bit_array(data'length)) := (others => NUL);
    variable value : ieee.numeric_bit.unsigned(data'length-1 downto 0) := ieee.numeric_bit.unsigned(data);
  begin
    for i in ret_val'reverse_range loop
      ret_val(i) := character'val(to_integer(value and to_unsigned(CODE_NB_VALUES-1, value'length)));
      value  := value srl CODE_LENGTH;
    end loop;
    return ret_val;
  end function;

  procedure decode_raw_bit_array(
    constant code   :     code_t;
    variable index  : inout code_index_t;
    variable result : out bit_array
  ) is
    variable ret_val : bit_array(code'length*CODE_LENGTH-1 downto 0);
    variable code_alias : code_t(index to code'high) := code;
  begin
    for i in code'range loop
      ret_val(
        (code_alias'length-i+1)*CODE_LENGTH - 1 downto (code_alias'length-i)*CODE_LENGTH
      ) := bit_array(ieee.numeric_bit.to_unsigned(character'pos(code_alias(i)), CODE_LENGTH));
    end loop;
    result := ret_val(result'length-1 downto 0);
    index := index + code_length_bit_array(result'length);
  end procedure;

  -- bit_array
  function encode_bit_array(data : bit_array) return code_t is
  begin
    if data'length = 0 then
      return ENCODED_NULL_RANGE;
    else
      return encode_range(data'left, data'right, data'ascending) & encode_raw_bit_array(data);
    end if;
  end function;

  procedure decode_bit_array(
    constant code   : in    code_t;
    variable index  : inout code_index_t;
    variable result : out   bit_array
  ) is
  begin
    index := index + CODE_LENGTH_RANGE_TYPE;
    decode_raw_bit_array(code, index, result);
  end procedure;

  -- bit_vector
  function encode_bit_vector(data : bit_vector) return code_t is
  begin
    if data'length = 0 then
      return ENCODED_NULL_RANGE;
    else
    return encode_bit_array(bit_array(data));
    end if;
  end function;

  procedure decode_bit_vector(
    constant code   : in    code_t;
    variable index  : inout code_index_t;
    variable result : out   bit_vector
  ) is
    variable ret_val : bit_array(result'range);
  begin
    decode_bit_array(code, index, ret_val);
    result := bit_vector(ret_val);
  end procedure;

  -- ieee.numeric_bit.unsigned
  function encode_numeric_bit_unsigned(data : ieee.numeric_bit.unsigned) return code_t is
  begin
    return encode_bit_array(bit_array(data));
  end function;

  procedure decode_numeric_bit_unsigned(
    constant code   : in    code_t;
    variable index  : inout code_index_t;
    variable result : out   ieee.numeric_bit.unsigned
  ) is
    variable ret_val : bit_array(result'range);
  begin
    decode_bit_array(code, index, ret_val);
    result := ieee.numeric_bit.unsigned(ret_val);
  end procedure;

  -- ieee.numeric_bit.signed
  function encode_numeric_bit_signed(data : ieee.numeric_bit.signed) return code_t is
  begin
    return encode_bit_array(bit_array(data));
  end function;

  procedure decode_numeric_bit_signed(
    constant code   : in    code_t;
    variable index  : inout code_index_t;
    variable result : out   ieee.numeric_bit.signed
  ) is
    variable ret_val : bit_array(result'range);
  begin
    decode_bit_array(code, index, ret_val);
    result := ieee.numeric_bit.signed(ret_val);
  end procedure;

  -- std_ulogic_array
  -- Function which transform a boolean into +1 or -1
  function idx_increment(is_ascending : boolean) return integer is
  begin
      if is_ascending then
        return 1;
      else
        return -1;
      end if;
  end function;

  function encode_std_ulogic_array(data : std_ulogic_array) return code_t is
    variable ret_val : code_t(1 to code_length_std_ulogic_array(data'length)) := (others => NUL);
    variable i       : integer  := data'left;
    variable byte    : natural;
    constant IDX_INCREMENT : integer := idx_increment(data'ascending);
  begin
    if data'length = 0 then
      return ENCODED_NULL_RANGE;
    end if;
    -- Encode the range
    ret_val(1 to CODE_LENGTH_RANGE_TYPE) := encode_range(data'left, data'right, data'ascending);
    -- One std_ulogic can represent LENGTH_STD_ULOGIC=9 value: it needs BITS_LENGTH_STD_ULOGIC=3 bits to store it.
    -- In a character (CODE_LENGTH=8 bits), we can store BITS_LENGTH_STD_ULOGIC/CODE_LENGTH=2 std_ulogic elements.
    for idx in CODE_LENGTH_RANGE_TYPE+1 to ret_val'high loop
      -- Encode the first std_ulogic
      byte := std_ulogic'pos(data(i));
      -- Encode the second std_ulogic (if not at the end of the std_ulogic_array)
      if i /= ret_val'right then
        i := i + IDX_INCREMENT;
        byte := byte + std_ulogic'pos(data(i)) * 2**BITS_LENGTH_STD_ULOGIC;
      end if;
      -- Convert into a charater and stores it into the string
      ret_val(idx) := character'val(byte);
      i := i + IDX_INCREMENT;
    end loop;
    return ret_val;
  end function;

  procedure decode_std_ulogic_array(
    constant code   : in    code_t;
    variable index  : inout code_index_t;
    variable result : out   std_ulogic_array
  ) is
    variable i : integer := result'left;
    variable upper_nibble : natural;
    constant CODE_LENGTH : positive := code_length_std_ulogic_array(result'length);
    constant IDX_INCREMENT : integer := idx_increment(result'ascending);
  begin
    index := index + CODE_LENGTH_RANGE_TYPE;

    for idx in index to index + CODE_LENGTH loop
      -- Decode the second std_ulogic
        if i /= result'right then
          upper_nibble := character'pos(code(idx))/(2**BITS_LENGTH_STD_ULOGIC);
          result(i + IDX_INCREMENT) := std_ulogic'val(upper_nibble);
        else
          upper_nibble := 0;
        end if;
      -- Decode the first std_ulogic
      result(i) := std_ulogic'val(character'pos(code(idx)) - upper_nibble*2**BITS_LENGTH_STD_ULOGIC);
      i := i + 2*IDX_INCREMENT;
    end loop;
    index := index + CODE_LENGTH;
  end procedure;

  -- std_ulogic_vector
  function encode_std_ulogic_vector(data : std_ulogic_vector) return code_t is
  begin
    return encode_std_ulogic_array(std_ulogic_array(data));
  end function;

  procedure decode_std_ulogic_vector(
    constant code   : in    code_t;
    variable index  : inout code_index_t;
    variable result : out   std_ulogic_vector
  ) is
    variable ret_val : std_ulogic_array(result'range);
  begin
    decode_std_ulogic_array(code, index, ret_val);
    result := std_ulogic_vector(ret_val);
  end procedure;

  -- ieee.numeric_std.unresolved_unsigned
  function encode_numeric_std_unsigned(data : ieee.numeric_std.unresolved_unsigned) return code_t is
  begin
    return encode_std_ulogic_array(std_ulogic_array(data));
  end function;

  procedure decode_numeric_std_unsigned(
    constant code   : in    code_t;
    variable index  : inout code_index_t;
    variable result : out   ieee.numeric_std.unresolved_unsigned
  ) is
    variable ret_val : std_ulogic_array(result'range);
  begin
    decode_std_ulogic_array(code, index, ret_val);
    result := ieee.numeric_std.unresolved_unsigned(ret_val);
  end procedure;

  -- ieee.numeric_std.signed
  function encode_numeric_std_signed(data : ieee.numeric_std.signed) return code_t is
  begin
    return encode_std_ulogic_array(std_ulogic_array(data));
  end function;

  procedure decode_numeric_std_signed(
    constant code   : in    code_t;
    variable index  : inout code_index_t;
    variable result : out   ieee.numeric_std.signed
  ) is
    variable ret_val : std_ulogic_array(result'range);
  begin
    decode_std_ulogic_array(code, index, ret_val);
    result := ieee.numeric_std.signed(ret_val);
  end procedure;


  -----------------------------------------------------------------------------
  -- Alternate decode procedures
  -----------------------------------------------------------------------------
  -- These function are only wrappers of the procedure with the same name


  -- Encode and Decode functions of predefined enumerated types

  function decode_boolean(constant code : code_t) return boolean is
    variable ret_val : boolean;
    variable index   : code_index_t := code'left;
  begin
    decode_boolean(code, index, ret_val);
    return ret_val;
  end function;

  function decode_character(constant code : code_t) return character is
    variable ret_val : character;
    variable index   : code_index_t := code'left;
  begin
    decode_character(code, index, ret_val);
    return ret_val;
  end function;

  function decode_bit(constant code : code_t) return bit is
    variable ret_val : bit;
    variable index   : code_index_t := code'left;
  begin
    decode_bit(code, index, ret_val);
    return ret_val;
  end function;

  function decode_std_ulogic(constant code : code_t) return std_ulogic is
    variable ret_val : std_ulogic;
    variable index   : code_index_t := code'left;
  begin
    decode_std_ulogic(code, index, ret_val);
    return ret_val;
  end function;

  function decode_severity_level(constant code : code_t) return severity_level is
    variable ret_val : severity_level;
    variable index   : code_index_t := code'left;
  begin
    decode_severity_level(code, index, ret_val);
    return ret_val;
  end function;

  function decode_file_open_kind(constant code : code_t) return file_open_kind is
    variable ret_val : file_open_kind;
    variable index   : code_index_t := code'left;
  begin
    decode_file_open_kind(code, index, ret_val);
    return ret_val;
  end function;

  function decode_file_open_status(constant code : code_t) return file_open_status is
    variable ret_val : file_open_status;
    variable index   : code_index_t := code'left;
  begin
    decode_file_open_status(code, index, ret_val);
    return ret_val;
  end function;


  -- Encode and Decode functions of predefined scalar types

  function decode_integer(constant code : code_t) return integer is
    variable ret_val : integer;
    variable index   : code_index_t := code'left;
  begin
    decode_integer(code, index, ret_val);
    return ret_val;
  end function;

  function decode_real(constant code : code_t) return real is
    variable ret_val : real;
    variable index   : code_index_t := code'left;
  begin
    decode_real(code, index, ret_val);
    return ret_val;
  end function;

  function decode_time(constant code : code_t) return time is
    variable ret_val : time;
    variable index   : code_index_t := code'left;
  begin
    decode_time(code, index, ret_val);
    return ret_val;
  end function;


  -- Encode and Decode functions of predefined composite types (records)

  function decode_complex(constant code : code_t) return complex is
    variable ret_val : complex;
    variable index   : code_index_t := code'left;
  begin
    decode_complex(code, index, ret_val);
    return ret_val;
  end function;

  function decode_complex_polar(constant code : code_t) return complex_polar is
    variable ret_val : complex_polar;
    variable index   : code_index_t := code'left;
  begin
    decode_complex_polar(code, index, ret_val);
    return ret_val;
  end function;


  -- Encode and decode functions of predefined composite types (arrays)

  function decode_string(code : code_t) return string is
    constant RET_RANGE : range_t := decode_range(code);
    variable ret_val : string(RET_RANGE'range) := (others => NUL);
    variable index : code_index_t := code'left;
  begin
    decode_string(code, index, ret_val);
    return ret_val;
  end function;

  function decode_raw_bit_array(code : code_t; length : positive) return bit_array is
    variable ret_val : bit_array(length-1 downto 0);
    variable index : code_index_t := code'left;
  begin
    decode_raw_bit_array(code, index, ret_val);
    return ret_val;
  end function;

  function decode_raw_bit_array(code : code_t) return bit_array is
  begin
    return decode_raw_bit_array(code, code'length*CODE_LENGTH);
  end function;

  function decode_bit_array(code : code_t) return bit_array is
    constant RET_RANGE : range_t := decode_range(code);
    variable ret_val : bit_array(RET_RANGE'range);
    variable index : code_index_t := code'left;
  begin
    decode_bit_array(code, index, ret_val);
    return ret_val;
  end function;

  function decode_bit_vector(code : code_t) return bit_vector is
    constant RET_RANGE : range_t := decode_range(code);
    variable ret_val : bit_vector(RET_RANGE'range);
    variable index : code_index_t := code'left;
  begin
    decode_bit_vector(code, index, ret_val);
    return ret_val;
  end function;

  function decode_numeric_bit_unsigned(code : code_t) return ieee.numeric_bit.unsigned is
    constant RET_RANGE : range_t := decode_range(code);
    variable ret_val : ieee.numeric_bit.unsigned(RET_RANGE'range);
    variable index : code_index_t := code'left;
  begin
    decode_numeric_bit_unsigned(code, index, ret_val);
    return ret_val;
  end function;

  function decode_numeric_bit_signed(code : code_t) return ieee.numeric_bit.signed is
    constant RET_RANGE : range_t := decode_range(code);
    variable ret_val : ieee.numeric_bit.signed(RET_RANGE'range);
    variable index : code_index_t := code'left;
  begin
    decode_numeric_bit_signed(code, index, ret_val);
    return ret_val;
  end function;

  function decode_std_ulogic_array(code : code_t) return std_ulogic_array is
    constant RET_RANGE : range_t := decode_range(code);
    variable ret_val : std_ulogic_array(RET_RANGE'range);
    variable index : code_index_t := code'left;
  begin
    decode_std_ulogic_array(code, index, ret_val);
    return ret_val;
  end function;

  function decode_std_ulogic_vector(code : code_t) return std_ulogic_vector is
    constant RET_RANGE : range_t := decode_range(code);
    variable ret_val : std_ulogic_vector(RET_RANGE'range);
    variable index : code_index_t := code'left;
  begin
    decode_std_ulogic_vector(code, index, ret_val);
    return ret_val;
  end function;

  function decode_numeric_std_unsigned(code : code_t) return ieee.numeric_std.unresolved_unsigned is
    constant RET_RANGE : range_t := decode_range(code);
    variable ret_val : ieee.numeric_std.unresolved_unsigned(RET_RANGE'range);
    variable index : code_index_t := code'left;
  begin
    decode_numeric_std_unsigned(code, index, ret_val);
    return ret_val;
  end function;

  function decode_numeric_std_signed(code : code_t) return ieee.numeric_std.signed is
    constant RET_RANGE : range_t := decode_range(code);
    variable ret_val : ieee.numeric_std.signed(RET_RANGE'range);
    variable index : code_index_t := code'left;
  begin
    decode_numeric_std_signed(code, index, ret_val);
    return ret_val;
  end function;


  -----------------------------------------------------------------------------
  -- Deprecated functions - Maintained for backward compatibility.
  -----------------------------------------------------------------------------

  -- Deprecated. Maintained for backward compatibility.
  function encode_array_header (
    constant range_left1   : code_t;
    constant range_right1  : code_t;
    constant is_ascending1 : code_t;
    constant range_left2   : code_t := "";
    constant range_right2  : code_t := "";
    constant is_ascending2 : code_t := "T"
  ) return code_t is
  begin
    assert False report
      "This function ('encode_array_header') is deprecated. Please use 'encode_range' from codec_v1993_pkg.vhd"
    severity warning;
    if range_left2 = "" then
      return range_left1 & range_right1 & is_ascending1;
    else
      return range_left1 & range_right1 & is_ascending1 & range_left2 & range_right2 & is_ascending2;
    end if;
  end function;

  -- Deprecated. Maintained for backward compatibility.
  function get_range(code : code_t) return range_t is
    constant range_left   : integer := decode_integer(code(code'left                       to code'left+CODE_LENGTH_INTEGER-1));
    constant range_right  : integer := decode_integer(code(code'left+CODE_LENGTH_INTEGER   to code'left+CODE_LENGTH_INTEGER*2-1));
    constant is_ascending : boolean := decode_boolean(code(code'left+CODE_LENGTH_INTEGER*2 to code'left+CODE_LENGTH_INTEGER*2+CODE_LENGTH_BOOLEAN));
    constant ret_val_ascending  : range_t(range_left to range_right) := (others => '0');
    constant ret_val_descending : range_t(range_left downto range_right) := (others => '0');
  begin
    assert False report
      "This function ('get_range') is deprecated. Please use 'decode_range' from codec_v1993_pkg.vhd"
    severity warning;
    return decode_range(code);
  end function;

  -- Deprecated. Maintained for backward compatibility.
  function to_byte_array(value : bit_vector) return code_t is
  begin
    assert False report
      "This function ('to_byte_array') is deprecated. Please use 'encode_raw_bit_array' from codec_v1993_pkg.vhd"
    severity warning;
    return encode_raw_bit_array(bit_array(value));
  end function;

  -- Deprecated. Maintained for backward compatibility.
  function from_byte_array(byte_array : code_t) return bit_vector is
  begin
    assert False report
      "This function ('from_byte_array') is deprecated. Please use 'decode_raw_bit_array' from codec_v1993_pkg.vhd"
    severity warning;
    return bit_vector(decode_raw_bit_array(byte_array));
  end function;

end package body;
