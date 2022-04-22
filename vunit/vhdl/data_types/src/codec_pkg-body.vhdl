-- This package contains support functions for standard codec building
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,
-- You can obtain one at http://mozilla.org/MPL/2.0/.
--
-- Copyright (c) 2014-2022, Lars Asplund lars.anders.asplund@gmail.com

library std;
use std.textio.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.math_complex.all;
use ieee.numeric_bit.all;
use ieee.numeric_std.all;
use ieee.math_real.all;


-------------------------------------------------------------------------------
-- Package body
-------------------------------------------------------------------------------
package body codec_pkg is

  --===========================================================================
  -- Encode and Decode procedures of predefined enumerated types
  --===========================================================================

  -----------------------------------------------------------------------------
  -- Boolean
  -----------------------------------------------------------------------------
  procedure encode_boolean(constant data : in boolean; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    if data then
      code(index) := 'T';
    else
      code(index) := 'F';
    end if;
    index := index + CODE_LENGTH_BOOLEAN;
  end procedure;

  procedure decode_boolean(constant code : in code_t; variable index : inout code_index_t; variable result : out boolean) is
  begin
    result := code(index) = 'T';
    index  := index + CODE_LENGTH_BOOLEAN;
  end procedure;

  -----------------------------------------------------------------------------
  -- Character
  -----------------------------------------------------------------------------
  procedure encode_character(constant data : in character; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    -- String start at index 1 (they are defined with a positive range)
    code(index) := data;
    index       := index + CODE_LENGTH_CHARACTER;
  end procedure;

  procedure decode_character(constant code : in code_t; variable index : inout code_index_t; variable result : out character) is
  begin
    result := code(index);
    index  := index + CODE_LENGTH_CHARACTER;
  end procedure;

  -----------------------------------------------------------------------------
  -- Bit
  -----------------------------------------------------------------------------
  procedure encode_bit(constant data : in bit; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    if data = '1' then
      code(index) := '1';
    else
      code(index) := '0';
    end if;
    index := index + CODE_LENGTH_BIT;
  end procedure;

  procedure decode_bit(constant code : in code_t; variable index : inout code_index_t; variable result : out bit) is
  begin
    if code(index) = '1' then
      result := '1';
    else
      result := '0';
    end if;
    index := index + CODE_LENGTH_BIT;
  end procedure;

  -----------------------------------------------------------------------------
  -- std_ulogic
  -----------------------------------------------------------------------------
  procedure encode_std_ulogic(constant data : in std_ulogic; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    -- The '2' is used to select the second character of the string representation
    code(index) := std_ulogic'image(data)(2);
    index       := index + CODE_LENGTH_STD_ULOGIC;
  end procedure;

  procedure decode_std_ulogic(constant code : in code_t; variable index : inout code_index_t; variable result : out std_ulogic) is
  begin
    result := std_ulogic'value("'" & code(index) & "'");
    index  := index + CODE_LENGTH_STD_ULOGIC;
  end procedure;

  -----------------------------------------------------------------------------
  -- severity_level
  -----------------------------------------------------------------------------
  procedure encode_severity_level(constant data : in severity_level; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    -- String start at index 1 (they are defined with a positive range)
    code(index) := character'val(severity_level'pos(data));
    index       := index + CODE_LENGTH_SEVERITY_LEVEL;
  end procedure;

  procedure decode_severity_level(constant code : in code_t; variable index : inout code_index_t; variable result : out severity_level) is
  begin
    result := severity_level'val(character'pos(code(index)));
    index  := index + CODE_LENGTH_SEVERITY_LEVEL;
  end procedure;

  -----------------------------------------------------------------------------
  -- file_open_kind
  -----------------------------------------------------------------------------
  procedure encode_file_open_kind(constant data : in file_open_kind; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    -- String start at index 1 (they are defined with a positive range)
    code(index) := character'val(file_open_kind'pos(data));
    index       := index + CODE_LENGTH_FILE_OPEN_KIND;
  end procedure;

  procedure decode_file_open_kind(constant code : in code_t; variable index : inout code_index_t; variable result : out file_open_kind) is
  begin
    result := file_open_kind'val(character'pos(code(index)));
    index  := index + CODE_LENGTH_FILE_OPEN_KIND;
  end procedure;

  -----------------------------------------------------------------------------
  -- file_open_status
  -----------------------------------------------------------------------------
  procedure encode_file_open_status(constant data : in file_open_status; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    -- String start at index 1 (they are defined with a positive range)
    code(index) := character'val(file_open_status'pos(data));
    index       := index + CODE_LENGTH_FILE_OPEN_STATUS;
  end procedure;

  procedure decode_file_open_status(constant code : in code_t; variable index : inout code_index_t; variable result : out file_open_status) is
  begin
    result := file_open_status'val(character'pos(code(index)));
    index  := index + CODE_LENGTH_FILE_OPEN_STATUS;
  end procedure;


  --===========================================================================
  -- Encode and Decode procedures of predefined scalar types
  --===========================================================================

  -----------------------------------------------------------------------------
  -- integer
  -----------------------------------------------------------------------------
  procedure encode_integer(constant data : in integer; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    code(index to index + CODE_LENGTH_INTEGER - 1) := encode_raw_bit_array(bit_array(ieee.numeric_bit.to_signed(data, SIMULATOR_INTEGER_WIDTH)));
    index := index + CODE_LENGTH_INTEGER;
  end procedure;

  procedure decode_integer(constant code : in code_t; variable index : inout code_index_t; variable result : out integer) is
  begin
    result := to_integer(ieee.numeric_bit.signed(
      decode_raw_bit_array(code(index to index + CODE_LENGTH_INTEGER - 1), SIMULATOR_INTEGER_WIDTH)
    ));
    index := index + CODE_LENGTH_INTEGER;
  end procedure;

  -----------------------------------------------------------------------------
  -- real
  -----------------------------------------------------------------------------
  procedure encode_real(constant data : in real; variable index : inout code_index_t; variable code : inout code_t) is
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

    encode_boolean(is_signed, index, code);
    encode_integer(exp, index, code);
    encode_integer(low, index, code);
    encode_integer(high, index, code);
  end procedure;

  procedure decode_real(constant code : in code_t; variable index : inout code_index_t; variable result : out real) is
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

  -----------------------------------------------------------------------------
  -- time
  -----------------------------------------------------------------------------
  procedure encode_time(constant data : in time; variable index : inout code_index_t; variable code : inout code_t) is

    function modulo(t : time; m : natural) return integer is
    begin
      return integer((t - (t/m)*m)/SIMULATOR_RESOLUTION) mod m;
    end function;

    variable t       : time;
    variable ascii   : natural;
  begin
    t := data;
    for i in index + CODE_LENGTH_TIME - 1 downto index loop
      ascii := modulo(t, CODE_NB_VALUES);
      code(i) := character'val(ascii);
      t := (t - (ascii * SIMULATOR_RESOLUTION))/CODE_NB_VALUES;
    end loop;
    index := index + CODE_LENGTH_TIME;
  end procedure;

  procedure decode_time(constant code : in code_t; variable index : inout code_index_t; variable result : out time) is
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
    result := r;
    index := index + CODE_LENGTH_TIME;
  end procedure;


  --===========================================================================
  -- Encode and Decode procedures of predefined composite types (records)
  --===========================================================================

  -----------------------------------------------------------------------------
  -- complex
  -----------------------------------------------------------------------------
  procedure encode_complex(constant data : in complex; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    encode_real(data.re, index, code);
    encode_real(data.im, index, code);
  end procedure;

  procedure decode_complex(constant code : in code_t; variable index : inout code_index_t; variable result : out complex) is
  begin
    decode_real(code, index, result.re);
    decode_real(code, index, result.im);
  end procedure;

  -----------------------------------------------------------------------------
  -- complex_polar
  -----------------------------------------------------------------------------
  procedure encode_complex_polar(constant data : in complex_polar; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    encode_real(data.mag, index, code);
    encode_real(data.arg, index, code);
  end procedure;

  procedure decode_complex_polar(constant code : in code_t; variable index : inout code_index_t; variable result : out complex_polar) is
  begin
    decode_real(code, index, result.mag);
    decode_real(code, index, result.arg);
  end procedure;


  --===========================================================================
  -- Encode and decode functions and procedures for range
  --===========================================================================

  procedure encode_range(
    constant range_left : integer;
    constant range_right : integer;
    constant is_ascending : boolean;
    variable index : inout code_index_t;
    variable code : inout code_t
  ) is
  begin
    encode_integer(range_left, index, code);
    encode_integer(range_right, index, code);
    encode_boolean(is_ascending, index, code);
  end procedure;

  function encode_range(range_left : integer; range_right : integer; is_ascending : boolean) return code_t is
    variable code  : code_t(1 to CODE_LENGTH_RANGE_TYPE);
    variable index : code_index_t := code'left;
  begin
    encode_range(range_left, range_right, is_ascending, index, code);
    return code;
  end function;

  function decode_range(code : code_t; index : code_index_t) return range_t is
    variable code_alias : code_t(1 to code'length-index-1) := code(code'left+index to code'right);
    constant RANGE_LEFT : integer := decode_integer(
      code_alias(1 to CODE_LENGTH_INTEGER)
    );
    constant RANGE_RIGHT : integer := decode_integer(
      code_alias(1 + CODE_LENGTH_INTEGER to CODE_LENGTH_INTEGER*2)
    );
    constant IS_ASCENDING : boolean := decode_boolean(
      code_alias(1 + CODE_LENGTH_INTEGER*2 to CODE_LENGTH_INTEGER*2 + CODE_LENGTH_BOOLEAN)
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

  function decode_range(code : code_t) return range_t is
  begin
    return decode_range(code, code'left);
  end function;

  -- Null range constant
  constant ENCODED_NULL_RANGE : code_t := encode_range(1, 0, true);


  --===========================================================================
  -- Functions which gives the number of code_t element to be used to encode the type
  --===========================================================================

  function code_length_string(length : natural) return natural is
  begin
    return CODE_LENGTH_RANGE_TYPE + length;
  end function;

  function code_length_raw_bit_array(length : natural) return natural is
  begin
    return ceil_div(length, CODE_LENGTH);
  end function;

  function code_length_bit_array(length : natural) return natural is
  begin
    return CODE_LENGTH_RANGE_TYPE + code_length_raw_bit_array(length);
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


  --===========================================================================
  -- Encode and decode procedures of predefined composite types (arrays)
  --===========================================================================

  -----------------------------------------------------------------------------
  -- string
  -----------------------------------------------------------------------------
  procedure encode_string(constant data : in string; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    -- Modelsim sets data'right to 0 which is out of the positive index
    -- range used by strings.
    if data'length = 0 then
      encode_range(data'left, data'right, data'ascending, index, code);
    else
      encode_range(data'left, data'right, data'ascending, index, code);
      code(index to index + data'length-1) := data;
      index := index + data'length;
    end if;
  end procedure;

  procedure decode_string(constant code : in code_t; variable index : inout code_index_t; variable result : out string) is
  begin
    index := index + CODE_LENGTH_RANGE_TYPE;
    result := code(index to index + code_length_string(result'length) - 1);
    index  := index + code_length_string(result'length);
  end procedure;

  -----------------------------------------------------------------------------
  -- raw_bit_array
  -----------------------------------------------------------------------------
  procedure encode_raw_bit_array(constant data : in bit_array; variable index : inout code_index_t; variable code : inout code_t) is
    variable value : ieee.numeric_bit.unsigned(data'length-1 downto 0) := ieee.numeric_bit.unsigned(data);
    variable code_alias : code_t(1 to code'length-1) := code(index to index+code'length-1);
  begin
    for i in code_alias'reverse_range loop
      code_alias(i) := character'val(to_integer(value and to_unsigned(CODE_NB_VALUES-1, value'length)));
      value := value srl CODE_LENGTH;
    end loop;
    index := index + code_length_raw_bit_array(data'length);
  end procedure;

  procedure decode_raw_bit_array(constant code : in code_t; variable index : inout code_index_t; variable result : out bit_array) is
    alias result_alias : bit_array(result'length-1 downto 0) is result;
    constant ACTUAL_CODE_LENGTH : positive := code_length_raw_bit_array(result'length);
    variable code_alias : code_t(1 to ACTUAL_CODE_LENGTH) := code(index to index+ACTUAL_CODE_LENGTH-1);
  begin
    for i in code_alias'range loop
      result_alias(
        (code_alias'length-i+1)*CODE_LENGTH-1 downto (code_alias'length-i)*CODE_LENGTH
      ) := bit_array(ieee.numeric_bit.to_unsigned(character'pos(code_alias(i)), CODE_LENGTH));
    end loop;
    index := index + code_length_raw_bit_array(result'length);
  end procedure;

  -----------------------------------------------------------------------------
  -- bit_array
  -----------------------------------------------------------------------------
  procedure encode_bit_array(constant data : in bit_array; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    if data'length = 0 then
      code(index to index + CODE_LENGTH_RANGE_TYPE - 1) := ENCODED_NULL_RANGE;
      index := index + CODE_LENGTH_RANGE_TYPE;
    else
      encode_range(data'left, data'right, data'ascending, index, code);
      encode_raw_bit_array(data, index, code);
    end if;
  end procedure;

  procedure decode_bit_array(constant code : in code_t; variable index : inout code_index_t; variable result : out bit_array) is
    constant RET_RANGE : range_t := decode_range(code, index);
  begin
    assert RET_RANGE'length = result'length report
      "The length of the actual array is different from what has been stored inside the codec"
    severity error;
    index := index + CODE_LENGTH_RANGE_TYPE;
    decode_raw_bit_array(code, index, result);
  end procedure;

  -----------------------------------------------------------------------------
  -- bit_vector
  -----------------------------------------------------------------------------
  procedure encode_bit_vector(constant data : in bit_vector; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    encode_bit_array(bit_array(data), index, code);
  end procedure;

  procedure decode_bit_vector(constant code : in code_t; variable index : inout code_index_t; variable result : out bit_vector) is
    variable ret_val : bit_array(result'range);
  begin
    decode_bit_array(code, index, ret_val);
    result := bit_vector(ret_val);
  end procedure;

  -----------------------------------------------------------------------------
  -- ieee.numeric_bit.unsigned
  -----------------------------------------------------------------------------
  procedure encode_numeric_bit_unsigned(constant data : in ieee.numeric_bit.unsigned; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    encode_bit_array(bit_array(data), index, code);
  end procedure;

  procedure decode_numeric_bit_unsigned(constant code : in code_t; variable index : inout code_index_t; variable result : out ieee.numeric_bit.unsigned) is
    variable ret_val : bit_array(result'range);
  begin
    decode_bit_array(code, index, ret_val);
    result := ieee.numeric_bit.unsigned(ret_val);
  end procedure;

  -----------------------------------------------------------------------------
  -- ieee.numeric_bit.signed
  -----------------------------------------------------------------------------
  procedure encode_numeric_bit_signed(constant data : in ieee.numeric_bit.signed; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    encode_bit_array(bit_array(data), index, code);
  end procedure;

  procedure decode_numeric_bit_signed(constant code : in code_t; variable index : inout code_index_t; variable result : out ieee.numeric_bit.signed) is
    variable ret_val : bit_array(result'range);
  begin
    decode_bit_array(code, index, ret_val);
    result := ieee.numeric_bit.signed(ret_val);
  end procedure;

  -----------------------------------------------------------------------------
  -- std_ulogic_array
  -----------------------------------------------------------------------------
  -- Function which transform a boolean into +1 or -1
  function idx_increment(is_ascending : boolean) return integer is
  begin
      if is_ascending then
        return 1;
      else
        return -1;
      end if;
  end function;

  procedure encode_std_ulogic_array(constant data : in std_ulogic_array; variable index : inout code_index_t; variable code : inout code_t) is
    constant ACTUAL_CODE_LENGTH : positive := code_length_std_ulogic_array(data'length);
    variable code_alias : code_t(1 to ACTUAL_CODE_LENGTH) := code(index to index+ACTUAL_CODE_LENGTH-1);
    variable i    : integer := data'left;
    variable byte : natural;
    constant IDX_INCREMENT : integer := idx_increment(data'ascending);
  begin
    if data'length = 0 then
      -- code_alias := encode_range(data'left, data'right, data'ascending);
      code_alias := ENCODED_NULL_RANGE;
    else
      -- Encode the range
      code_alias(1 to CODE_LENGTH_RANGE_TYPE) := encode_range(data'left, data'right, data'ascending);
      -- One std_ulogic can represent LENGTH_STD_ULOGIC=9 value: it needs BITS_LENGTH_STD_ULOGIC=3 bits to store it.
      -- In a character (CODE_LENGTH=8 bits), we can store BITS_LENGTH_STD_ULOGIC/CODE_LENGTH=2 std_ulogic elements.
      for idx in CODE_LENGTH_RANGE_TYPE+1 to code_alias'high loop
        -- Encode the first std_ulogic
        byte := std_ulogic'pos(data(i));
        -- Encode the second std_ulogic (if not at the end of the std_ulogic_array)
        if i /= code_alias'right then
          i := i + IDX_INCREMENT;
          byte := byte + std_ulogic'pos(data(i)) * 2**BITS_LENGTH_STD_ULOGIC;
        end if;
        -- Convert into a charater and stores it into the string
        code_alias(idx) := character'val(byte);
        i := i + IDX_INCREMENT;
      end loop;
    end if;
    index := index + ACTUAL_CODE_LENGTH;
  end procedure;

  procedure decode_std_ulogic_array(constant code : in code_t; variable index : inout code_index_t; variable result : out std_ulogic_array) is
    variable i : integer := result'left;
    variable upper_nibble : natural;
    constant ACTUAL_CODE_LENGTH : positive := code_length_std_ulogic_array(result'length);
    constant IDX_INCREMENT : integer := idx_increment(result'ascending);
  begin
    index := index + CODE_LENGTH_RANGE_TYPE;
    for idx in index to index + ACTUAL_CODE_LENGTH loop
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
    index := index + ACTUAL_CODE_LENGTH;
  end procedure;

  -----------------------------------------------------------------------------
  -- std_ulogic_vector
  -----------------------------------------------------------------------------
  procedure encode_std_ulogic_vector(constant data : in std_ulogic_vector; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    encode_std_ulogic_array(std_ulogic_array(data), index, code);
  end procedure;

  procedure decode_std_ulogic_vector(constant code : in code_t; variable index : inout code_index_t; variable result : out std_ulogic_vector) is
    variable ret_val : std_ulogic_array(result'range);
  begin
    decode_std_ulogic_array(code, index, ret_val);
    result := std_ulogic_vector(ret_val);
  end procedure;

  -----------------------------------------------------------------------------
  -- ieee.numeric_std.unresolved_unsigned
  -----------------------------------------------------------------------------
  procedure encode_numeric_std_unsigned(constant data : in ieee.numeric_std.unresolved_unsigned; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    encode_std_ulogic_array(std_ulogic_array(data), index, code);
  end procedure;

  procedure decode_numeric_std_unsigned(constant code : in code_t; variable index : inout code_index_t; variable result : out ieee.numeric_std.unresolved_unsigned) is
    variable ret_val : std_ulogic_array(result'range);
  begin
    decode_std_ulogic_array(code, index, ret_val);
    result := ieee.numeric_std.unresolved_unsigned(ret_val);
  end procedure;

  -----------------------------------------------------------------------------
  -- ieee.numeric_std.unresolved_signed
  -----------------------------------------------------------------------------
  procedure encode_numeric_std_signed(constant data : in ieee.numeric_std.unresolved_signed; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    encode_std_ulogic_array(std_ulogic_array(data), index, code);
  end procedure;

  procedure decode_numeric_std_signed(constant code : in code_t; variable index : inout code_index_t; variable result : out ieee.numeric_std.unresolved_signed) is
    variable ret_val : std_ulogic_array(result'range);
  begin
    decode_std_ulogic_array(code, index, ret_val);
    result := ieee.numeric_std.unresolved_signed(ret_val);
  end procedure;


  --===========================================================================
  -- Encode functions
  --===========================================================================
  -- These function are only wrappers of the procedure with the same name

  -----------------------------------------------------------------------------
  -- Encode functions of predefined enumerated types
  -----------------------------------------------------------------------------

  function encode_boolean(data : boolean) return code_t is
    variable ret_val : code_t(1 to CODE_LENGTH_BOOLEAN);
    variable index   : code_index_t := ret_val'left;
  begin
    encode_boolean(data, index, ret_val);
    return ret_val;
  end function;

  function encode_character(data : character) return code_t is
    variable ret_val : code_t(1 to CODE_LENGTH_CHARACTER);
    variable index   : code_index_t := ret_val'left;
  begin
    encode_character(data, index, ret_val);
    return ret_val;
  end function;

  function encode_bit(data : bit) return code_t is
    variable ret_val : code_t(1 to CODE_LENGTH_BIT);
    variable index   : code_index_t := ret_val'left;
  begin
    encode_bit(data, index, ret_val);
    return ret_val;
  end function;

  function encode_std_ulogic(data : std_ulogic) return code_t is
    variable ret_val : code_t(1 to CODE_LENGTH_STD_ULOGIC);
    variable index   : code_index_t := ret_val'left;
  begin
    encode_std_ulogic(data, index, ret_val);
    return ret_val;
  end function;

  function encode_severity_level(data : severity_level) return code_t is
    variable ret_val : code_t(1 to CODE_LENGTH_SEVERITY_LEVEL);
    variable index   : code_index_t := ret_val'left;
  begin
    encode_severity_level(data, index, ret_val);
    return ret_val;
  end function;

  function encode_file_open_kind(data : file_open_kind) return code_t is
    variable ret_val : code_t(1 to CODE_LENGTH_FILE_OPEN_KIND);
    variable index   : code_index_t := ret_val'left;
  begin
    encode_file_open_kind(data, index, ret_val);
    return ret_val;
  end function;

  function encode_file_open_status(data : file_open_status) return code_t is
    variable ret_val : code_t(1 to CODE_LENGTH_FILE_OPEN_STATUS);
    variable index   : code_index_t := ret_val'left;
  begin
    encode_file_open_status(data, index, ret_val);
    return ret_val;
  end function;


  -----------------------------------------------------------------------------
  -- Encode functions of predefined scalar types
  -----------------------------------------------------------------------------

  function encode_integer(data : integer) return code_t is
    variable ret_val : code_t(1 to CODE_LENGTH_INTEGER);
    variable index   : code_index_t := ret_val'left;
  begin
    encode_integer(data, index, ret_val);
    return ret_val;
  end function;

  function encode_real(data : real) return code_t is
    variable ret_val : code_t(1 to CODE_LENGTH_REAL);
    variable index   : code_index_t := ret_val'left;
  begin
    encode_real(data, index, ret_val);
    return ret_val;
  end function;

  function encode_time(data : time) return code_t is
    variable ret_val : code_t(1 to CODE_LENGTH_TIME);
    variable index   : code_index_t := ret_val'left;
  begin
    encode_time(data, index, ret_val);
    return ret_val;
  end function;


  -----------------------------------------------------------------------------
  -- Encode functions of predefined composite types (records)
  -----------------------------------------------------------------------------

  function encode_complex(data : complex) return code_t is
    variable ret_val : code_t(1 to CODE_LENGTH_COMPLEX);
    variable index   : code_index_t := ret_val'left;
  begin
    encode_complex(data, index, ret_val);
    return ret_val;
  end function;

  function encode_complex_polar(data : complex_polar) return code_t is
    variable ret_val : code_t(1 to CODE_LENGTH_COMPLEX_POLAR);
    variable index   : code_index_t := ret_val'left;
  begin
    encode_complex_polar(data, index, ret_val);
    return ret_val;
  end function;


  -----------------------------------------------------------------------------
  -- Encode functions of predefined composite types (arrays)
  -----------------------------------------------------------------------------

  function encode_string(data : string) return code_t is
    variable ret_val : code_t(1 to code_length_string(data'length));
    variable index : code_index_t := ret_val'left;
  begin
    encode_string(data, index, ret_val);
    return ret_val;
  end function;

  function encode_raw_bit_array(data : bit_array) return code_t is
    variable ret_val : code_t(1 to code_length_raw_bit_array(data'length));
    variable index : code_index_t := ret_val'left;
  begin
    encode_raw_bit_array(data, index, ret_val);
    return ret_val;
  end function;

  function encode_bit_array(data : bit_array) return code_t is
    variable ret_val : code_t(1 to code_length_bit_array(data'length));
    variable index : code_index_t := ret_val'left;
  begin
    encode_bit_array(data, index, ret_val);
    return ret_val;
  end function;

  function encode_bit_vector(data : bit_vector) return code_t is
    variable ret_val : code_t(1 to code_length_string(data'length));
    variable index : code_index_t := ret_val'left;
  begin
    encode_bit_vector(data, index, ret_val);
    return ret_val;
  end function;

  function encode_numeric_bit_unsigned(data : ieee.numeric_bit.unsigned) return code_t is
    variable ret_val : code_t(1 to code_length_numeric_bit_unsigned(data'length));
    variable index : code_index_t := ret_val'left;
  begin
    encode_numeric_bit_unsigned(data, index, ret_val);
    return ret_val;
  end function;

  function encode_numeric_bit_signed(data : ieee.numeric_bit.signed) return code_t is
    variable ret_val : code_t(1 to code_length_numeric_bit_signed(data'length));
    variable index : code_index_t := ret_val'left;
  begin
    encode_numeric_bit_signed(data, index, ret_val);
    return ret_val;
  end function;

  function encode_std_ulogic_array(data : std_ulogic_array) return code_t is
    variable ret_val : code_t(1 to code_length_std_ulogic_array(data'length));
    variable index : code_index_t := ret_val'left;
  begin
    encode_std_ulogic_array(data, index, ret_val);
    return ret_val;
  end function;

  function encode_std_ulogic_vector(data : std_ulogic_vector) return code_t is
    variable ret_val : code_t(1 to code_length_std_ulogic_vector(data'length));
    variable index : code_index_t := ret_val'left;
  begin
    encode_std_ulogic_vector(data, index, ret_val);
    return ret_val;
  end function;

  function encode_numeric_std_unsigned(data : ieee.numeric_std.unresolved_unsigned) return code_t is
    variable ret_val : code_t(1 to code_length_numeric_std_unsigned(data'length));
    variable index : code_index_t := ret_val'left;
  begin
    encode_numeric_std_unsigned(data, index, ret_val);
    return ret_val;
  end function;

  function encode_numeric_std_signed(data : ieee.numeric_std.unresolved_signed) return code_t is
    variable ret_val : code_t(1 to code_length_numeric_std_signed(data'length));
    variable index : code_index_t := ret_val'left;
  begin
    encode_numeric_std_signed(data, index, ret_val);
    return ret_val;
  end function;


  --===========================================================================
  -- Decode functions
  --===========================================================================
  -- These function are only wrappers of the procedure with the same name

  -----------------------------------------------------------------------------
  -- Decode functions of predefined enumerated types
  -----------------------------------------------------------------------------

  function decode_boolean(code : code_t) return boolean is
    variable ret_val : boolean;
    variable index   : code_index_t := code'left;
  begin
    decode_boolean(code, index, ret_val);
    return ret_val;
  end function;

  function decode_character(code : code_t) return character is
    variable ret_val : character;
    variable index   : code_index_t := code'left;
  begin
    decode_character(code, index, ret_val);
    return ret_val;
  end function;

  function decode_bit(code : code_t) return bit is
    variable ret_val : bit;
    variable index   : code_index_t := code'left;
  begin
    decode_bit(code, index, ret_val);
    return ret_val;
  end function;

  function decode_std_ulogic(code : code_t) return std_ulogic is
    variable ret_val : std_ulogic;
    variable index   : code_index_t := code'left;
  begin
    decode_std_ulogic(code, index, ret_val);
    return ret_val;
  end function;

  function decode_severity_level(code : code_t) return severity_level is
    variable ret_val : severity_level;
    variable index   : code_index_t := code'left;
  begin
    decode_severity_level(code, index, ret_val);
    return ret_val;
  end function;

  function decode_file_open_kind(code : code_t) return file_open_kind is
    variable ret_val : file_open_kind;
    variable index   : code_index_t := code'left;
  begin
    decode_file_open_kind(code, index, ret_val);
    return ret_val;
  end function;

  function decode_file_open_status(code : code_t) return file_open_status is
    variable ret_val : file_open_status;
    variable index   : code_index_t := code'left;
  begin
    decode_file_open_status(code, index, ret_val);
    return ret_val;
  end function;


  -----------------------------------------------------------------------------
  -- Decode functions of predefined scalar types
  -----------------------------------------------------------------------------

  function decode_integer(code : code_t) return integer is
    variable ret_val : integer;
    variable index   : code_index_t := code'left;
  begin
    decode_integer(code, index, ret_val);
    return ret_val;
  end function;

  function decode_real(code : code_t) return real is
    variable ret_val : real;
    variable index   : code_index_t := code'left;
  begin
    decode_real(code, index, ret_val);
    return ret_val;
  end function;

  function decode_time(code : code_t) return time is
    variable ret_val : time;
    variable index   : code_index_t := code'left;
  begin
    decode_time(code, index, ret_val);
    return ret_val;
  end function;


  -----------------------------------------------------------------------------
  -- Decode functions of predefined composite types (records)
  -----------------------------------------------------------------------------

  function decode_complex(code : code_t) return complex is
    variable ret_val : complex;
    variable index   : code_index_t := code'left;
  begin
    decode_complex(code, index, ret_val);
    return ret_val;
  end function;

  function decode_complex_polar(code : code_t) return complex_polar is
    variable ret_val : complex_polar;
    variable index   : code_index_t := code'left;
  begin
    decode_complex_polar(code, index, ret_val);
    return ret_val;
  end function;


  -----------------------------------------------------------------------------
  -- Decode functions of predefined composite types (arrays)
  -----------------------------------------------------------------------------

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

  function decode_numeric_std_signed(code : code_t) return ieee.numeric_std.unresolved_signed is
    constant RET_RANGE : range_t := decode_range(code);
    variable ret_val : ieee.numeric_std.unresolved_signed(RET_RANGE'range);
    variable index : code_index_t := code'left;
  begin
    decode_numeric_std_signed(code, index, ret_val);
    return ret_val;
  end function;


  --===========================================================================
  -- Deprecated functions - Maintained for backward compatibility.
  --===========================================================================

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
