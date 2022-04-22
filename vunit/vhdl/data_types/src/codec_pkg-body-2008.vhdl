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
use ieee.fixed_pkg.all;
use ieee.float_pkg.all;

library work;
use work.codec_pkg.all;


-------------------------------------------------------------------------------
-- Package body
-------------------------------------------------------------------------------
package body codec_pkg_2008 is

  --===========================================================================
  -- Encode and Decode procedures of new predefined types from VHDL 2008
  --===========================================================================

  -----------------------------------------------------------------------------
  -- boolean_vector
  -----------------------------------------------------------------------------
  procedure encode_boolean_vector(constant data : in boolean_vector; variable index : inout code_index_t; variable code : out code_t) is
    variable ret_val : bit_array(data'range);
  begin
    for i in data'range loop
      if data(i) = true then
        ret_val(i) := '1';
      else
        ret_val(i) := '0';
      end if;
    end loop;
    encode_bit_array(ret_val, index, code);
  end procedure;

  procedure decode_boolean_vector(constant code : in code_t; variable index : inout code_index_t; variable result : out boolean_vector) is
    variable ret_val : bit_array(result'range);
  begin
    decode_bit_array(code, index, ret_val);
    for i in ret_val'range loop
      if ret_val(i) = '1' then
        result(i) := true;
      else
        result(i) := false;
      end if;
    end loop;
  end procedure;

  -----------------------------------------------------------------------------
  -- integer_vector
  -----------------------------------------------------------------------------
  procedure encode_integer_vector(constant data : in integer_vector; variable index : inout code_index_t; variable code : out code_t) is
  begin
    if data'length = 0 then
      code(index to index + CODE_LENGTH_RANGE_TYPE - 1) := ENCODED_NULL_RANGE;
    else
      encode_range(data'left, data'right, data'ascending, index, code);
      for i in data'range loop
        encode_integer(data(i), index, code);
      end loop;
    end if;
  end procedure;

  procedure decode_integer_vector(constant code : in code_t; variable index : inout code_index_t; variable result : out integer_vector) is
  begin
    index := index + CODE_LENGTH_RANGE_TYPE;
    for i in result'range loop
      decode_integer(code, index, result(i));
    end loop;
  end procedure;

  -----------------------------------------------------------------------------
  -- real_vector
  -----------------------------------------------------------------------------
  procedure encode_real_vector(constant data : in real_vector; variable index : inout code_index_t; variable code : out code_t) is
  begin
    encode_range(data'left, data'right, data'ascending, index, code);
    for i in data'range loop
      encode_real(data(i), index, code);
    end loop;
  end procedure;

  procedure decode_real_vector(constant code : in code_t; variable index : inout code_index_t; variable result : out real_vector) is
  begin
    index := index + CODE_LENGTH_RANGE_TYPE;
    for i in result'range loop
      decode_real(code, index, result(i));
    end loop;
  end procedure;

  -----------------------------------------------------------------------------
  -- time_vector
  -----------------------------------------------------------------------------
  procedure encode_time_vector(constant data : in time_vector; variable index : inout code_index_t; variable code : out code_t) is
  begin
    encode_range(data'left, data'right, data'ascending, index, code);
    for i in data'range loop
      encode_time(data(i), index, code);
    end loop;
  end procedure;

  procedure decode_time_vector(constant code : in code_t; variable index : inout code_index_t; variable result : out time_vector) is
  begin
    index := index + CODE_LENGTH_RANGE_TYPE;
    for i in result'range loop
      decode_time(code, index, result(i));
    end loop;
  end procedure;

  -----------------------------------------------------------------------------
  -- ufixed
  -----------------------------------------------------------------------------
  procedure encode_ufixed(constant data : in ufixed; variable index : inout code_index_t; variable code : out code_t) is
  begin
    encode_std_ulogic_array(std_ulogic_array(data), index, code);
  end procedure;

  procedure decode_ufixed(constant code : in code_t; variable index : inout code_index_t; variable result : out ufixed) is
    variable ret_val : std_ulogic_array(result'range);
  begin
    decode_std_ulogic_array(code, index, ret_val);
    result := ufixed(ret_val);
  end procedure;

  -----------------------------------------------------------------------------
  -- sfixed
  -----------------------------------------------------------------------------
  procedure encode_sfixed(constant data : in sfixed; variable index : inout code_index_t; variable code : out code_t) is
  begin
    encode_std_ulogic_array(std_ulogic_array(data), index, code);
  end procedure;

  procedure decode_sfixed(constant code : in code_t; variable index : inout code_index_t; variable result : out sfixed) is
    variable ret_val : std_ulogic_array(result'range);
  begin
    decode_std_ulogic_array(code, index, ret_val);
    result := sfixed(ret_val);
  end procedure;

  -----------------------------------------------------------------------------
  -- float
  -----------------------------------------------------------------------------
  procedure encode_float(constant data : in float; variable index : inout code_index_t; variable code : out code_t) is
  begin
    encode_std_ulogic_array(std_ulogic_array(data), index, code);
  end procedure;

  procedure decode_float(constant code : in code_t; variable index : inout code_index_t; variable result : out float) is
    variable ret_val : std_ulogic_array(result'range);
  begin
    decode_std_ulogic_array(code, index, ret_val);
    result := float(ret_val);
  end procedure;


  --===========================================================================
  -- Functions which gives the number of code_t element to be used to encode the type
  --===========================================================================

  function code_length_boolean_vector(length : natural) return natural is
  begin
    return code_length_bit_array(length);
  end function;

  function code_length_integer_vector(length : natural) return natural is
  begin
    return CODE_LENGTH_RANGE_TYPE + code_length_integer * length;
  end function;

  function code_length_real_vector(length : natural) return natural is
  begin
    return CODE_LENGTH_RANGE_TYPE + code_length_real * length;
  end function;

  function code_length_time_vector(length : natural) return natural is
  begin
    return CODE_LENGTH_RANGE_TYPE + code_length_time * length;
  end function;

  function code_length_ufixed(length : natural) return natural is
  begin
    return code_length_std_ulogic_array(length);
  end function;

  function code_length_sfixed(length : natural) return natural is
  begin
    return code_length_std_ulogic_array(length);
  end function;

  function code_length_float(length : natural) return natural is
  begin
    return code_length_std_ulogic_array(length);
  end function;

  --===========================================================================
  -- Encode functions of new predefined types from VHDL 2008
  --===========================================================================

  function encode_boolean_vector(data : boolean_vector) return code_t is
    variable ret_val : code_t(1 to code_length_boolean_vector(data'length));
    variable index : code_index_t := ret_val'left;
  begin
    encode_boolean_vector(data, index, ret_val);
    return ret_val;
  end function;

  function encode_integer_vector(data : integer_vector) return code_t is
    variable ret_val : code_t(1 to code_length_integer_vector(data'length));
    variable index : code_index_t := ret_val'left;
  begin
    encode_integer_vector(data, index, ret_val);
    return ret_val;
  end function;

  function encode_real_vector(data : real_vector) return code_t is
    variable ret_val : code_t(1 to code_length_real_vector(data'length));
    variable index : code_index_t := ret_val'left;
  begin
    encode_real_vector(data, index, ret_val);
    return ret_val;
  end function;

  function encode_time_vector(data : time_vector) return code_t is
    variable ret_val : code_t(1 to code_length_time_vector(data'length));
    variable index : code_index_t := ret_val'left;
  begin
    encode_time_vector(data, index, ret_val);
    return ret_val;
  end function;

  function encode_ufixed(data : ufixed) return code_t is
    variable ret_val : code_t(1 to code_length_ufixed(data'length));
    variable index : code_index_t := ret_val'left;
  begin
    encode_ufixed(data, index, ret_val);
    return ret_val;
  end function;

  function encode_sfixed(data : sfixed) return code_t is
    variable ret_val : code_t(1 to code_length_sfixed(data'length));
    variable index : code_index_t := ret_val'left;
  begin
    encode_sfixed(data, index, ret_val);
    return ret_val;
  end function;

  function encode_float(data : float) return code_t is
    variable ret_val : code_t(1 to code_length_float(data'length));
    variable index : code_index_t := ret_val'left;
  begin
    encode_float(data, index, ret_val);
    return ret_val;
  end function;


  --===========================================================================
  -- Decode functions of new predefined types from VHDL 2008
  --===========================================================================

  function decode_boolean_vector(code : code_t) return boolean_vector is
    constant RET_RANGE : range_t := decode_range(code);
    variable ret_val : boolean_vector(RET_RANGE'range);
    variable index : code_index_t := code'left;
  begin
    decode_boolean_vector(code, index, ret_val);
    return ret_val;
  end function;

  function decode_integer_vector(code : code_t) return integer_vector is
    constant RET_RANGE : range_t := decode_range(code);
    variable ret_val : integer_vector(RET_RANGE'range);
    variable index : code_index_t := code'left;
  begin
    decode_integer_vector(code, index, ret_val);
    return ret_val;
  end function;

  function decode_real_vector(code : code_t) return real_vector is
    constant RET_RANGE : range_t := decode_range(code);
    variable ret_val : real_vector(RET_RANGE'range);
    variable index : code_index_t := code'left;
  begin
    decode_real_vector(code, index, ret_val);
    return ret_val;
  end function;

  function decode_time_vector(code : code_t) return time_vector is
    constant RET_RANGE : range_t := decode_range(code);
    variable ret_val : time_vector(RET_RANGE'range);
    variable index : code_index_t := code'left;
  begin
    decode_time_vector(code, index, ret_val);
    return ret_val;
  end function;

  function decode_ufixed(code : code_t) return ufixed is
    constant RET_RANGE : range_t := decode_range(code);
    variable ret_val : ufixed(RET_RANGE'range);
    variable index : code_index_t := code'left;
  begin
    decode_ufixed(code, index, ret_val);
    return ret_val;
  end function;

  function decode_sfixed(code : code_t) return sfixed is
    constant RET_RANGE : range_t := decode_range(code);
    variable ret_val : sfixed(RET_RANGE'range);
    variable index : code_index_t := code'left;
  begin
    decode_sfixed(code, index, ret_val);
    return ret_val;
  end function;

  function decode_float(code : code_t) return float is
    constant RET_RANGE : range_t := decode_range(code);
    variable ret_val : float(RET_RANGE'range);
    variable index : code_index_t := code'left;
  begin
    decode_float(code, index, ret_val);
    return ret_val;
  end function;

end package body;
