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
  -- Base types and functions
  -----------------------------------------------------------------------------

  function to_byte_array( --TODO
    constant value : bit_vector
  )
    return string is
    variable ret_val   : string(1 to (value'length + 7) / 8);
    variable value_int : ieee.numeric_bit.unsigned(value'length - 1 downto 0) := ieee.numeric_bit.unsigned(value);
  begin
    for i in ret_val'reverse_range loop
      ret_val(i) := character'val(to_integer(value_int and to_unsigned(255, value_int'length)));
      value_int  := value_int srl 8;
    end loop;
    return ret_val;
  end function;

  function from_byte_array( --TODO
    constant byte_array : string)
    return bit_vector is
    constant byte_array_int : string(1 to byte_array'length) := byte_array;
    variable ret_val        : bit_vector(byte_array'length*8-1 downto 0);
  begin
    for i in byte_array_int'range loop
      ret_val((byte_array_int'length-i)*8 + 7 downto (byte_array_int'length-i)*8) := bit_vector(ieee.numeric_bit.to_unsigned(character'pos(byte_array_int(i)), 8));
    end loop;
    return ret_val;
  end function;


  -----------------------------------------------------------------------------
  -- Encode and Decode functions of predefined enumerated types
  -----------------------------------------------------------------------------

  -- Boolean
  function encode_boolean(constant data : boolean) return code_vec_t is
  begin
    if data then
      return "T";
    else
      return "F";
    end if;
  end function;

  procedure decode_boolean(
    constant code   :       code_vec_t;
    variable index  : inout code_index_t;
    variable result : out   boolean
  ) is
  begin
    result := code(index) = 'T';
    index  := index + CODE_LENGTH_BOOLEAN;
  end procedure;

  function decode_boolean(constant code : code_vec_t) return boolean is
    variable ret_val : boolean;
    variable index   : code_index_t := code'left;
  begin
    decode_boolean(code, index, ret_val);
    return ret_val;
  end function;

  -- Character
  function encode_character(constant data : character) return code_vec_t is
  begin
    return (CODE_VEC_LOW => data);
  end function;

  procedure decode_character(
    constant code   :       code_vec_t;
    variable index  : inout code_index_t;
    variable result : out   character
  ) is
  begin
    result := code(index);
    index  := index + CODE_LENGTH_CHARACTER;
  end procedure;

  function decode_character(constant code : code_vec_t) return character is
    variable ret_val : character;
    variable index   : code_index_t := code'left;
  begin
    decode_character(code, index, ret_val);
    return ret_val;
  end function;

  -- Bit
  function encode_bit(constant data : bit) return code_vec_t is
  begin
    if data = '1' then
      return "1";
    else
      return "0";
    end if;
  end function;

  procedure decode_bit(
    constant code   :       code_vec_t;
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

  function decode_bit(constant code : code_vec_t) return bit is
    variable ret_val : bit;
    variable index   : code_index_t := code'left;
  begin
    decode_bit(code, index, ret_val);
    return ret_val;
  end function;

  -- std_ulogic
  function encode_std_ulogic(constant data : std_ulogic) return code_vec_t is
  begin
    -- The '2 to 2' is used to select the second character of the string representation
    return std_ulogic'image(data)(2 to 2);
  end function;

  procedure decode_std_ulogic(
    constant code   :       code_vec_t;
    variable index  : inout code_index_t;
    variable result : out   std_ulogic
  ) is
  begin
    result := std_ulogic'value("'" & code(index) & "'");
    index  := index + CODE_LENGTH_STD_ULOGIC;
  end procedure;

  function decode_std_ulogic(constant code : code_vec_t) return std_ulogic is
    variable ret_val : std_ulogic;
    variable index   : code_index_t := code'left;
  begin
    decode_std_ulogic(code, index, ret_val);
    return ret_val;
  end function;

  -- severity_level
  function encode_severity_level(constant data : severity_level) return code_vec_t is
  begin
    return (CODE_VEC_LOW => character'val(severity_level'pos(data)));
  end function;

  procedure decode_severity_level(
    constant code   :       code_vec_t;
    variable index  : inout code_index_t;
    variable result : out   severity_level
  ) is
  begin
    result := severity_level'val(character'pos(code(index)));
    index  := index + CODE_LENGTH_SEVERITY_LEVEL;
  end procedure;

  function decode_severity_level(constant code : code_vec_t) return severity_level is
    variable ret_val : severity_level;
    variable index   : code_index_t := code'left;
  begin
    decode_severity_level(code, index, ret_val);
    return ret_val;
  end function;

  -- file_open_kind
  function encode_file_open_kind(constant data : file_open_kind) return code_vec_t is
  begin
    return (CODE_VEC_LOW => character'val(file_open_kind'pos(data)));
  end function;

  procedure decode_file_open_kind(
    constant code   :       code_vec_t;
    variable index  : inout code_index_t;
    variable result : out   file_open_kind
  ) is
  begin
    result := file_open_kind'val(character'pos(code(index)));
    index  := index + CODE_LENGTH_FILE_OPEN_KIND;
  end procedure;

  function decode_file_open_kind(constant code : code_vec_t) return file_open_kind is
    variable ret_val : file_open_kind;
    variable index   : code_index_t := code'left;
  begin
    decode_file_open_kind(code, index, ret_val);
    return ret_val;
  end function;

  -- file_open_status
  function encode_file_open_status(constant data : file_open_status) return code_vec_t is
  begin
    return (CODE_VEC_LOW => character'val(file_open_status'pos(data)));
  end function;

  procedure decode_file_open_status(
    constant code   :       code_vec_t;
    variable index  : inout code_index_t;
    variable result : out   file_open_status
  ) is
  begin
    result := file_open_status'val(character'pos(code(index)));
    index  := index + CODE_LENGTH_FILE_OPEN_STATUS;
  end procedure;

  function decode_file_open_status(constant code : code_vec_t) return file_open_status is
    variable ret_val : file_open_status;
    variable index   : code_index_t := code'left;
  begin
    decode_file_open_status(code, index, ret_val);
    return ret_val;
  end function;


  -----------------------------------------------------------------------------
  -- Encode and Decode functions of predefined scalar types
  -----------------------------------------------------------------------------

  -- integer
  function encode_integer(constant data : integer) return code_vec_t is
  begin
    return to_byte_array(bit_vector(ieee.numeric_bit.to_signed(data, 32))); -- TODO (remove 32)
  end function;

  procedure decode_integer(
    constant code   :       code_vec_t;
    variable index  : inout code_index_t;
    variable result : out   integer
  ) is
  begin
    result := to_integer(ieee.numeric_bit.signed(from_byte_array(code(index to index + CODE_LENGTH_INTEGER-1)))); --TODO
    index  := index + CODE_LENGTH_INTEGER;
  end procedure;

  function decode_integer(constant code : code_vec_t) return integer is
    variable ret_val : integer;
    variable index   : code_index_t := code'left;
  begin
    decode_integer(code, index, ret_val);
    return ret_val;
  end function;

  -- real
  function encode_real(constant data : real) return code_vec_t is
    constant is_signed : boolean := data < 0.0;
    variable val : real := data;
    variable exp : integer;
    variable low : integer;
    variable high : integer;

    function log2(a : real) return integer is -- TODO
      variable y : real;
      variable n : integer := 0;
    begin
      if a = 1.0 or a = 0.0 then
        return 0;
      end if;
      y := a;
      if(a > 1.0) then
        while y >= 2.0 loop
          y := y / 2.0;
          n := n + 1;
        end loop;
        return n;
      else
        while y < 1.0 loop
          y := y * 2.0;
          n := n - 1;
        end loop;
        return n;
      end if;
    end function;

  begin
    if is_signed then
      val := -val;
    end if;
    exp := log2(val);
    -- Assume 53 mantissa bits TODO
    val := val * 2.0 ** (-exp + 53);
    high := integer(floor(val * 2.0 ** (-31)));
    low := integer(val - real(high) * 2.0 ** 31);
    return encode_boolean(is_signed) & encode_integer(exp) & encode_integer(low) & encode_integer(high);
  end function;

  procedure decode_real(
    constant code   :       code_vec_t;
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

  function decode_real(constant code : code_vec_t) return real is
    variable ret_val : real;
    variable index   : code_index_t := code'left;
  begin
    decode_real(code, index, ret_val);
    return ret_val;
  end function;

  -- time
  function encode_time(constant data : time) return code_vec_t is
    function modulo(t : time; m : natural) return integer is -- TODO
    begin
      return integer((t - (t/m)*m)/SIMULATOR_RESOLUTION) mod m;
    end function;

    variable ret_val : string(1 to CODE_LENGTH_TIME);
    variable t       : time;
    variable ascii   : natural;
  begin
    -- TODO assumes time is time_code_length bytes
    t := data;
    for i in CODE_LENGTH_TIME downto 1 loop
      ascii := modulo(t, 256);
      ret_val(i) := character'val(ascii);
      t          := (t - (ascii * SIMULATOR_RESOLUTION))/256;
    end loop;
    return ret_val;
  end function;

  procedure decode_time(
    constant code   :       code_vec_t;
    variable index  : inout code_index_t;
    variable result : out   time
  ) is
    constant code_int : string(1 to CODE_LENGTH_TIME) := code(index to index + CODE_LENGTH_TIME - 1);
    variable r : time;
    variable b : integer;
  begin
    -- TODO assumes time is TIME_CODE_LENGTH bytes
    r := SIMULATOR_RESOLUTION * 0;

    for i in code_int'range loop
      b := character'pos(code_int(i));
      r := r * 256;
      if i = 1 and b >= 128 then
        b := b - 256;
      end if;
      r := r + b * SIMULATOR_RESOLUTION;
    end loop;

    index := index + CODE_LENGTH_TIME;
    result := r;
  end procedure;

  function decode_time(constant code : code_vec_t) return time is
    variable ret_val : time;
    variable index   : code_index_t := code'left;
  begin
    decode_time(code, index, ret_val);
    return ret_val;
  end function;


  -----------------------------------------------------------------------------
  -- Encode and Decode functions of predefined composite types (records)
  -----------------------------------------------------------------------------

  -- complex
  function encode_complex(constant data : complex) return code_vec_t is
  begin
    return encode_real(data.re) & encode_real(data.im);
  end function;

  procedure decode_complex(
    constant code   :       code_vec_t;
    variable index  : inout code_index_t;
    variable result : out   complex
  ) is
  begin
    decode_real(code, index, result.re);
    decode_real(code, index, result.im);
  end procedure;

  function decode_complex(constant code : code_vec_t) return complex is
    variable ret_val : complex;
    variable index   : code_index_t := code'left;
  begin
    decode_complex(code, index, ret_val);
    return ret_val;
  end function;

  -- complex_polar
  function encode_complex_polar(constant data : complex_polar) return code_vec_t is
  begin
    return encode_real(data.mag) & encode_real(data.arg);
  end function;

  procedure decode_complex_polar(
    constant code   :       code_vec_t;
    variable index  : inout code_index_t;
    variable result : out   complex_polar
  ) is
  begin
    decode_real(code, index, result.mag);
    decode_real(code, index, result.arg);
  end procedure;

  function decode_complex_polar(constant code : code_vec_t) return complex_polar is
    variable ret_val : complex_polar;
    variable index   : code_index_t := code'left;
  begin
    decode_complex_polar(code, index, ret_val);
    return ret_val;
  end function;



  -----------------------------------------------------------------------------
  -- TODO
  -----------------------------------------------------------------------------

  function encode_range(
    constant range_left   : integer;
    constant range_right  : integer;
    constant is_ascending : boolean
  ) return code_vec_t is
  begin
    return encode_integer(range_left) & encode_integer(range_right) & encode_boolean(is_ascending);
  end function;

  function decode_range(
    constant code  : code_vec_t;
    constant index : code_index_t := 0
  ) return range_t is
    constant RANGE_LEFT : integer := decode_integer(
      code(index to index + CODE_LENGTH_INTEGER - 1)
    );
    constant RANGE_RIGHT : integer := decode_integer(
      code(index + CODE_LENGTH_INTEGER to index + CODE_LENGTH_INTEGER*2 - 1)
    );
    constant IS_ASCENDING : boolean := decode_boolean(
      code(index + CODE_LENGTH_INTEGER*2 to index + CODE_LENGTH_INTEGER*2 + CODE_LENGTH_BOOLEAN - 1)
    );
    constant RET_VAL_ASCENDING  : range_t(RANGE_LEFT to RANGE_RIGHT) := (others => '0');
    constant RET_VAL_DESCENDING : range_t(RANGE_LEFT downto RANGE_RIGHT) := (others => '0');
  begin
    if IS_ASCENDING then
      return RET_VAL_ASCENDING;
    else
      return RET_VAL_DESCENDING;
    end if;
  end function;

  procedure decode_range(
    constant code   :       code_vec_t;
    variable index  : inout code_index_t;
    variable result : out   range_t
  ) is
  begin
    decode_range(code, index, result);
    index := index + CODE_LENGTH_RANGE;
  end procedure;





  -----------------------------------------------------------------------------
  -- Alternate decode procedures
  -----------------------------------------------------------------------------



  -----------------------------------------------------------------------------
  -- Deprecated functions - Maintained for backward compatibility.
  -----------------------------------------------------------------------------

  -- Deprecated. Maintained for backward compatibility.
  function encode_array_header (
    constant range_left1   : code_vec_t;
    constant range_right1  : code_vec_t;
    constant is_ascending1 : code_vec_t;
    constant range_left2   : code_vec_t := "";
    constant range_right2  : code_vec_t := "";
    constant is_ascending2 : code_vec_t := "T"
  ) return code_vec_t is
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
  function get_range(
    constant code : code_vec_t
  ) return range_t is
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

end package body;
