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
use ieee.math_real.all;
use ieee.math_complex.all;
use ieee.std_logic_1164.all;
use ieee.numeric_bit.all;
use ieee.numeric_std.all;

library work;
use work.common_pkg.all;


-------------------------------------------------------------------------------
-- Package declaration
-------------------------------------------------------------------------------
package codec_builder_pkg is

  -- This packages enables the user to encode any predefined type into a unique type.
  -- This unique type is a 'string' (array of 'character').
  -- The functionality can be used to build a queue capable of storing different
  -- types in it (see the VUnit 'queue' package)
  alias code_t is string;

  -- Remark: this package encode any predefined type into a string, however, this
  -- package is not meant for serialization and deserialization of data accross
  -- versions of VUnit. Only the following API is considered for compatibility
  -- between VUnit version.


  --===========================================================================
  -- API for the ADVANCED USERS
  --===========================================================================

  -----------------------------------------------------------------------------
  -- Base types and constants describing how the encoding is performed
  -----------------------------------------------------------------------------

  -- A character (string of length 1) stores value on 8 bits
  constant code_length : positive := 8;
  -- A character (string of length 1) can store up to 2**8 = 256 value
  constant code_nb_values : positive := 2**code_length;


  -----------------------------------------------------------------------------
  -- VUnit defined types
  -----------------------------------------------------------------------------

  -- The ieee.std_ulogic_vector is defined with a natural range.
  -- If you need to encode an array of ieee.std_ulogic (or an array of any subtype
  -- of ieee.std_ulogic) with an integer range, you can use the type 'std_ulogic_array'
  type bit_array is array(integer range <>) of bit;

  -- The std.bit_vector is defined with a natural range.
  -- If you need to encode an array of std.bit (or an array of any subtype
  -- of std.bit) with an integer range, you can use the type 'bit_array'
  type std_ulogic_array is array(integer range <>) of std_ulogic;


  -----------------------------------------------------------------------------
  -- Encoding length for each types
  -----------------------------------------------------------------------------
  -- If you need to retrieve the length of the encoded data whitout,
  -- encoding it, you can use these constants/functions:

  -- Encoding length of predefined enumerated types:
  -- The formulation "type'pos(type'right) + 1" gives the number of element of the enumerated type
  constant length_boolean          : positive := boolean'pos(boolean'right) + 1;
  constant length_character        : positive := character'pos(character'right) + 1;
  constant length_bit              : positive := bit'pos(bit'right) + 1;
  constant length_std_ulogic       : positive := std_ulogic'pos(std_ulogic'right) + 1;
  constant length_severity_level   : positive := severity_level'pos(severity_level'right) + 1;
  constant length_file_open_kind   : positive := file_open_kind'pos(file_open_kind'right) + 1;
  constant length_file_open_status : positive := file_open_status'pos(file_open_status'right) + 1;

  -- Encoding length of predefined enumerated types:
  constant code_length_boolean          : positive := ceil_div(length_boolean, code_length);
  constant code_length_character        : positive := ceil_div(length_character, code_length);
  constant code_length_bit              : positive := ceil_div(length_bit, code_length);
  constant code_length_std_ulogic       : positive := ceil_div(length_std_ulogic, code_length);
  constant code_length_severity_level   : positive := ceil_div(length_severity_level, code_length);
  constant code_length_file_open_kind   : positive := ceil_div(length_file_open_kind, code_length);
  constant code_length_file_open_status : positive := ceil_div(length_file_open_status, code_length);

  -- Encoding length of predefined scalar types:
  constant code_length_integer : positive := simulator_integer_width/code_length;
  constant code_length_real    : positive := code_length_boolean + 3 * code_length_integer;
  constant code_length_time    : positive := simulator_time_width/code_length;

  -- Encoding length of predefined composite types (records):
  constant code_length_complex       : positive := 2 * code_length_real;
  constant code_length_complex_polar : positive := 2 * code_length_real;

  -- Encoding length of predefined composite types (arrays):
  -- These functions give you the length of the encoded array depending on the
  -- length of the array to encode
  function code_length_string(length : natural) return natural;
  function code_length_bit_vector(length : natural) return natural;
  function code_length_numeric_bit_unsigned(length : natural) return natural;
  function code_length_numeric_bit_signed(length : natural) return natural;
  function code_length_std_ulogic_vector(length : natural) return natural;
  function code_length_numeric_std_unsigned(length : natural) return natural;
  function code_length_numeric_std_signed(length : natural) return natural;

  -- Encoding length of array types defined in this package:
  -- These functions give you the length of the encoded array depending on the
  -- length of the array to encode
  function code_length_bit_array(length : natural) return natural;
  function code_length_std_ulogic_array(length : natural) return natural;



  --===========================================================================
  -- API for the VUnit DEVELOPERS
  --===========================================================================

  -- This section present low level procedures to encode and decode. They are not
  -- intented to be used by the casual user.
  -- These are intended for VUnit developers (and advanced users) to build encode
  -- and decode procedures and functions of more complex types.

  -- In most of the procedures, there are:
  --  * the 'code' parameter is the encoded data.
  --  * the 'index' parameter which indicates from where inside the 'code' parameter
  --    we must encode the data or decode the data.
  -- The 'index' is update by the procdeures in order for the next encode/decode
  -- procedure to be abla to keep encoding or decoding the data without having to deal
  -- with the length of the internal representation.
  -- The implementations on the 'encode_complex' and 'decode_complex' is an example
  -- of that feature. We first encode/decode the real part, then the imaginary part.

  -- Index to track the position of an encoded element inside an instance of code_t
  alias code_index_t is integer;


  -----------------------------------------------------------------------------
  -- Encoding of predefined composite types (arrays)
  -----------------------------------------------------------------------------

  -- Two things need to be extracted from an array to encode it:
  --  * The range of the array
  --  * The data inside the array
  -- The range encoding is performed by 'encode_range' and 'decode_range' functions.

  -- A range is constituted of two bounds (an left bound and a right bound)
  -- We also need to store the ascending/descending attribute to when the
  -- range is of lenght 1 or when the range is null
  constant code_length_range_type : positive := 2 * code_length_integer + code_length_boolean;

  -- This type is used so that we can return an array with any integer range.
  -- It is not meant to carry any other information.
  type range_t is array(integer range <>) of bit;

  -- Encode and decode functions for range
  procedure encode_range(
    constant range_left : integer;
    constant range_right : integer;
    constant is_ascending : boolean;
    variable index : inout code_index_t;
    variable code : inout code_t
  );
  alias encode is encode_range[integer, integer, boolean, code_index_t, code_t];
  -- Note, there is no procedure 'decode_range' because we want to retireve an unknown range.
  -- If we had a procedure, we would need to provide a variable to the parameter 'result' which
  -- must be constrained. This contradict the purpose of the functionnality.
  -- procedure decode_range(constant code : in code_t; variable index : inout code_index_t; variable result : out range_t);
  -- There are a 'decode_range' function inside the codec.vhd package


  -----------------------------------------------------------------------------
  -- Encoding of 'raw' bit_vector and 'raw' std_ulogic_vector
  -----------------------------------------------------------------------------
  -- We define two functions which encode a 'bit_vector' or a 'std_ulogic_vector' without its range.

  -- This function is used to return the encode value of a bit_array without its range encoded.
  -- To encode/decode bit_array, use encode_bit_array and decode_bit_array.
  function encode_raw_bit_array(data : bit_array) return code_t;
  function decode_raw_bit_array(code : code_t) return bit_array;
  function decode_raw_bit_array(code : code_t; length : positive) return bit_array;
  procedure encode_raw_bit_array(constant data : in bit_array; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_raw_bit_array(constant code : in code_t; variable index : inout code_index_t; variable result : out bit_array);
  -- Note some of these functions and procedures does not have aliases 'encode' and
  -- 'decode' as they are homograph with the 'encode_bit_array' and
  -- 'decode_bit_array' functions and procedures

  -- This function gives you the length of the encoded array depending on the
  -- length of the array to encode
  function code_length_raw_bit_array(length : natural) return natural;

  -- This function is used to return the encode value of a std_ulogic_array without its range encoded.
  -- To encode/decode std_ulogic_array, use encode_std_ulogic_array and decode_std_ulogic_array.
  function encode_raw_std_ulogic_array(data : std_ulogic_array) return code_t;
  function decode_raw_std_ulogic_array(code : code_t) return std_ulogic_array;
  function decode_raw_std_ulogic_array(code : code_t; length : positive) return std_ulogic_array;
  procedure encode_raw_std_ulogic_array(constant data : in std_ulogic_array; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_raw_std_ulogic_array(constant code : in code_t; variable index : inout code_index_t; variable result : out std_ulogic_array);
  -- Note some of these functions and procedures does not have aliases 'encode' and
  -- 'decode' as they are homograph with the 'encode_std_ulogic_array' and
  -- 'decode_std_ulogic_array' functions and procedures

  -- This function gives you the length of the encoded array depending on the
  -- length of the array to encode
  function code_length_raw_std_ulogic_array(length : natural) return natural;


  -----------------------------------------------------------------------------
  -- Alternate encode and decode procedures
  -----------------------------------------------------------------------------

  -- Predefined enumerated types
  procedure encode_boolean(constant data : in boolean; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_boolean(constant code : in code_t; variable index : inout code_index_t; variable result : out boolean);
  alias encode is encode_boolean[boolean, code_index_t, code_t];
  alias decode is decode_boolean[code_t, code_index_t, boolean];

  procedure encode_character(constant data : in character; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_character(constant code : in code_t; variable index : inout code_index_t; variable result : out character);
  alias encode is encode_character[character, code_index_t, code_t];
  alias decode is decode_character[code_t, code_index_t, character];

  procedure encode_bit(constant data : in bit; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_bit(constant code : in code_t; variable index : inout code_index_t; variable result : out bit);
  alias encode is encode_bit[bit, code_index_t, code_t];
  alias decode is decode_bit[code_t, code_index_t, bit];

  procedure encode_std_ulogic(constant data : in std_ulogic; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_std_ulogic(constant code : in code_t; variable index : inout code_index_t; variable result : out std_ulogic);
  alias encode is encode_std_ulogic[std_ulogic, code_index_t, code_t];
  alias decode is decode_std_ulogic[code_t, code_index_t, std_ulogic];

  procedure encode_severity_level(constant data : in severity_level; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_severity_level(constant code : in code_t; variable index : inout code_index_t; variable result : out severity_level);
  alias encode is encode_severity_level[severity_level, code_index_t, code_t];
  alias decode is decode_severity_level[code_t, code_index_t, severity_level];

  procedure encode_file_open_kind(constant data : in file_open_kind; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_file_open_kind(constant code : in code_t; variable index : inout code_index_t; variable result : out file_open_kind);
  alias encode is encode_file_open_kind[file_open_kind, code_index_t, code_t];
  alias decode is decode_file_open_kind[code_t, code_index_t, file_open_kind];

  procedure encode_file_open_status(constant data : in file_open_status; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_file_open_status(constant code : in code_t; variable index : inout code_index_t; variable result : out file_open_status);
  alias encode is encode_file_open_status[file_open_status, code_index_t, code_t];
  alias decode is decode_file_open_status[code_t, code_index_t, file_open_status];

  -- Predefined scalar types
  procedure encode_integer(constant data : in integer; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_integer(constant code : in code_t; variable index : inout code_index_t; variable result : out integer);
  alias encode is encode_integer[integer, code_index_t, code_t];
  alias decode is decode_integer[code_t, code_index_t, integer];

  procedure encode_real(constant data : in real; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_real(constant code : in code_t; variable index : inout code_index_t; variable result : out real);
  alias encode is encode_real[real, code_index_t, code_t];
  alias decode is decode_real[code_t, code_index_t, real];

  procedure encode_time(constant data : in time; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_time(constant code : in code_t; variable index : inout code_index_t; variable result : out time);
  alias encode is encode_time[time, code_index_t, code_t];
  alias decode is decode_time[code_t, code_index_t, time];

  -- Predefined composite types (records)
  procedure encode_complex(constant data : in complex; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_complex(constant code : in code_t; variable index : inout code_index_t; variable result : out complex);
  alias encode is encode_complex[complex, code_index_t, code_t];
  alias decode is decode_complex[code_t, code_index_t, complex];

  procedure encode_complex_polar(constant data : in complex_polar; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_complex_polar(constant code : in code_t; variable index : inout code_index_t; variable result : out complex_polar);
  alias encode is encode_complex_polar[complex_polar, code_index_t, code_t];
  alias decode is decode_complex_polar[code_t, code_index_t, complex_polar];

  -- Predefined composite types (arrays)
  procedure encode_string(constant data : in string; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_string(constant code : in code_t; variable index : inout code_index_t; variable result : out string);
  alias encode is encode_string[string, code_index_t, code_t];
  alias decode is decode_string[code_t, code_index_t, string];

  procedure encode_bit_array(constant data : in bit_array; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_bit_array(constant code : in code_t; variable index : inout code_index_t; variable result : out bit_array);
  alias encode is encode_bit_array[bit_array, code_index_t, code_t];
  alias decode is decode_bit_array[code_t, code_index_t, bit_array];

  procedure encode_bit_vector(constant data : in bit_vector; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_bit_vector(constant code : in code_t; variable index : inout code_index_t; variable result : out bit_vector);
  alias encode is encode_bit_vector[bit_vector, code_index_t, code_t];
  alias decode is decode_bit_vector[code_t, code_index_t, bit_vector];

  procedure encode_numeric_bit_unsigned(constant data : in ieee.numeric_bit.unsigned; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_numeric_bit_unsigned(constant code : in code_t; variable index : inout code_index_t; variable result : out ieee.numeric_bit.unsigned);
  alias encode is encode_numeric_bit_unsigned[ieee.numeric_bit.unsigned, code_index_t, code_t];
  alias decode is decode_numeric_bit_unsigned[code_t, code_index_t, ieee.numeric_bit.unsigned];

  procedure encode_numeric_bit_signed(constant data : in ieee.numeric_bit.signed; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_numeric_bit_signed(constant code : in code_t; variable index : inout code_index_t; variable result : out ieee.numeric_bit.signed);
  alias encode is encode_numeric_bit_signed[ieee.numeric_bit.signed, code_index_t, code_t];
  alias decode is decode_numeric_bit_signed[code_t, code_index_t, ieee.numeric_bit.signed];

  procedure encode_std_ulogic_array(constant data : in std_ulogic_array; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_std_ulogic_array(constant code : in code_t; variable index : inout code_index_t; variable result : out std_ulogic_array);
  alias encode is encode_std_ulogic_array[std_ulogic_array, code_index_t, code_t];
  alias decode is decode_std_ulogic_array[code_t, code_index_t, std_ulogic_array];

  procedure encode_std_ulogic_vector(constant data : in std_ulogic_vector; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_std_ulogic_vector(constant code : in code_t; variable index : inout code_index_t; variable result : out std_ulogic_vector);
  alias encode is encode_std_ulogic_vector[std_ulogic_vector, code_index_t, code_t];
  alias decode is decode_std_ulogic_vector[code_t, code_index_t, std_ulogic_vector];

  procedure encode_numeric_std_unsigned(constant data : in ieee.numeric_std.unresolved_unsigned; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_numeric_std_unsigned(constant code : in code_t; variable index : inout code_index_t; variable result : out ieee.numeric_std.unresolved_unsigned);
  alias encode is encode_numeric_std_unsigned[ieee.numeric_std.unresolved_unsigned, code_index_t, code_t];
  alias decode is decode_numeric_std_unsigned[code_t, code_index_t, ieee.numeric_std.unresolved_unsigned];

  procedure encode_numeric_std_signed(constant data : in ieee.numeric_std.unresolved_signed; variable index : inout code_index_t; variable code : inout code_t);
  procedure decode_numeric_std_signed(constant code : in code_t; variable index : inout code_index_t; variable result : out ieee.numeric_std.unresolved_signed);
  alias encode is encode_numeric_std_signed[ieee.numeric_std.unresolved_signed, code_index_t, code_t];
  alias decode is decode_numeric_std_signed[code_t, code_index_t, ieee.numeric_std.unresolved_signed];



  --===========================================================================
  -- Deprecated functions. Maintained for backward compatibility
  --===========================================================================

  alias integer_code_length          is code_length_integer;
  alias boolean_code_length          is code_length_boolean;
  alias real_code_length             is code_length_real;
  alias std_ulogic_code_length       is code_length_std_ulogic;
  alias bit_code_length              is code_length_bit;
  alias time_code_length             is code_length_time;
  alias severity_level_code_length   is code_length_severity_level;
  alias file_open_status_code_length is code_length_file_open_status;
  alias file_open_kind_code_length   is code_length_file_open_kind;
  alias complex_code_length          is code_length_complex;
  alias complex_polar_code_length    is code_length_complex_polar;

  -- This function is deprecated.
  -- Use the 'encode_range' function instead.
  -- If you need to encode two ranges, make two call to the 'encode_range' function.
  function encode_array_header(
    constant range_left1   : in code_t;
    constant range_right1  : in code_t;
    constant is_ascending1 : in code_t;
    constant range_left2   : in code_t := "";
    constant range_right2  : in code_t := "";
    constant is_ascending2 : in code_t := "T"
  ) return code_t;

  -- This function is deprecated.
  -- Use the 'encode_raw_bit_array' function instead.
  function to_byte_array(value : bit_vector) return code_t;

  -- This function is deprecated.
  -- Use the 'decode_raw_bit_array' function instead.
  function from_byte_array(byte_array : code_t) return bit_vector;

end package;




-------------------------------------------------------------------------------
-- Package body
-------------------------------------------------------------------------------
package body codec_builder_pkg is

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
    index := index + code_length_boolean;
  end procedure;

  procedure decode_boolean(constant code : in code_t; variable index : inout code_index_t; variable result : out boolean) is
  begin
    result := code(index) = 'T';
    index  := index + code_length_boolean;
  end procedure;

  -----------------------------------------------------------------------------
  -- Character
  -----------------------------------------------------------------------------
  procedure encode_character(constant data : in character; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    code(index) := data;
    index       := index + code_length_character;
  end procedure;

  procedure decode_character(constant code : in code_t; variable index : inout code_index_t; variable result : out character) is
  begin
    result := code(index);
    index  := index + code_length_character;
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
    index := index + code_length_bit;
  end procedure;

  procedure decode_bit(constant code : in code_t; variable index : inout code_index_t; variable result : out bit) is
  begin
    if code(index) = '1' then
      result := '1';
    else
      result := '0';
    end if;
    index := index + code_length_bit;
  end procedure;

  -----------------------------------------------------------------------------
  -- std_ulogic
  -----------------------------------------------------------------------------
  procedure encode_std_ulogic(constant data : in std_ulogic; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    -- The '2' is used to select the second character of the string representation
    code(index) := std_ulogic'image(data)(2);
    index       := index + code_length_std_ulogic;
  end procedure;

  procedure decode_std_ulogic(constant code : in code_t; variable index : inout code_index_t; variable result : out std_ulogic) is
  begin
    result := std_ulogic'value("'" & code(index) & "'");
    index  := index + code_length_std_ulogic;
  end procedure;

  -----------------------------------------------------------------------------
  -- severity_level
  -----------------------------------------------------------------------------
  procedure encode_severity_level(constant data : in severity_level; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    code(index) := character'val(severity_level'pos(data));
    index       := index + code_length_severity_level;
  end procedure;

  procedure decode_severity_level(constant code : in code_t; variable index : inout code_index_t; variable result : out severity_level) is
  begin
    result := severity_level'val(character'pos(code(index)));
    index  := index + code_length_severity_level;
  end procedure;

  -----------------------------------------------------------------------------
  -- file_open_kind
  -----------------------------------------------------------------------------
  procedure encode_file_open_kind(constant data : in file_open_kind; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    code(index) := character'val(file_open_kind'pos(data));
    index       := index + code_length_file_open_kind;
  end procedure;

  procedure decode_file_open_kind(constant code : in code_t; variable index : inout code_index_t; variable result : out file_open_kind) is
  begin
    result := file_open_kind'val(character'pos(code(index)));
    index  := index + code_length_file_open_kind;
  end procedure;

  -----------------------------------------------------------------------------
  -- file_open_status
  -----------------------------------------------------------------------------
  procedure encode_file_open_status(constant data : in file_open_status; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    code(index) := character'val(file_open_status'pos(data));
    index       := index + code_length_file_open_status;
  end procedure;

  procedure decode_file_open_status(constant code : in code_t; variable index : inout code_index_t; variable result : out file_open_status) is
  begin
    result := file_open_status'val(character'pos(code(index)));
    index  := index + code_length_file_open_status;
  end procedure;


  --===========================================================================
  -- Encode and Decode procedures of predefined scalar types
  --===========================================================================

  -----------------------------------------------------------------------------
  -- integer
  -----------------------------------------------------------------------------
  procedure encode_integer(constant data : in integer; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    code(index to index + code_length_integer - 1) := encode_raw_bit_array(bit_array(ieee.numeric_bit.to_signed(data, simulator_integer_width)));
    index := index + code_length_integer;
  end procedure;

  procedure decode_integer(constant code : in code_t; variable index : inout code_index_t; variable result : out integer) is
  begin
    result := to_integer(ieee.numeric_bit.signed(
      decode_raw_bit_array(code(index to index + code_length_integer - 1), simulator_integer_width)
    ));
    index := index + code_length_integer;
  end procedure;

  -----------------------------------------------------------------------------
  -- real
  -----------------------------------------------------------------------------
  -- TODO Remove hard coded values (EXPONENT, MANTISSE and SIGN width constants are defined in the common_pkg)
  -- TODO Add automatic 64 bits support
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
    value := value * 2.0 ** (-exp + 53);
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

    ret_val := (real(low) + real(high) * 2.0**31) * 2.0 ** (exp - 53);
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
      return integer((t - (t/m)*m)/simulator_resolution) mod m;
    end function;

    variable t       : time;
    variable ascii   : natural;
  begin
    t := data;
    for i in index + code_length_time - 1 downto index loop
      ascii := modulo(t, code_nb_values);
      code(i) := character'val(ascii);
      t := (t - (ascii * simulator_resolution)) / code_nb_values;
    end loop;
    index := index + code_length_time;
  end procedure;

  procedure decode_time(constant code : in code_t; variable index : inout code_index_t; variable result : out time) is
    constant code_int : code_t(1 to code_length_time) := code(index to index + code_length_time - 1);
    variable r : time;
    variable b : integer;
  begin
    r := simulator_resolution * 0;
    for i in code_int'range loop
      b := character'pos(code_int(i));
      r := r * code_nb_values;
      if i = 1 and b >= code_nb_values/2 then
        b := b - code_nb_values;
      end if;
      r := r + b * simulator_resolution;
    end loop;
    result := r;
    index := index + code_length_time;
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


  --===========================================================================
  -- Functions which gives the number of code_t element to be used to encode the type
  --===========================================================================

  function code_length_string(length : natural) return natural is
  begin
    return code_length_range_type + length;
  end function;

  function code_length_raw_bit_array(length : natural) return natural is
  begin
    -- bit_array are encoded inte string. The array of bits is divided into
    -- chunks of code_length=8 bits (gives a number N between 0 and 255) which
    -- can be directly used to select the Nth character in the string type.
    return ceil_div(length, code_length);
  end function;

  function code_length_bit_array(length : natural) return natural is
  begin
    return code_length_range_type + code_length_raw_bit_array(length);
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

  constant bits_length_std_ulogic : positive := positive(ceil(log2(real(length_std_ulogic))));
  function code_length_raw_std_ulogic_array(length : natural) return natural is
  begin
    -- std_ulogic_array are encoded into string. The array is divided into
    -- groups of 2 std_ulogic.
    -- One std_ulogic can represent length_std_ulogic=9 value: it needs bits_length_std_ulogic=4 bits to store it.
    -- In a character (code_length=8 bits), we can store code_length/bits_length_std_ulogic=2 std_ulogic elements.
    return ceil_div(length, code_length/bits_length_std_ulogic);
  end function;

  function code_length_std_ulogic_array(length : natural) return natural is
  begin
    return code_length_range_type + code_length_raw_std_ulogic_array(length);
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
    -- Note: Modelsim sets data'right to 0 which is out of the positive
    -- index range used by strings.
    encode_range(data'left, data'right, data'ascending, index, code);
    if data'length /= 0 then
      code(index to index + data'length-1) := data;
      index := index + data'length;
    end if;
  end procedure;

  procedure decode_string(constant code : in code_t; variable index : inout code_index_t; variable result : out string) is
  begin
    index := index + code_length_range_type;
    result := code(index to index + result'length - 1);
    index  := index + code_length_string(result'length);
  end procedure;

  -----------------------------------------------------------------------------
  -- raw_bit_array
  -----------------------------------------------------------------------------
  procedure encode_raw_bit_array(constant data : in bit_array; variable index : inout code_index_t; variable code : inout code_t) is
    constant actual_code_length : natural := code_length_raw_bit_array(data'length);
    variable value : ieee.numeric_bit.unsigned(data'length-1 downto 0) := ieee.numeric_bit.unsigned(data);
    constant BYTE_MASK : ieee.numeric_bit.unsigned(data'length-1 downto 0) := resize(to_unsigned(code_nb_values-1, code_length), data'length);
  begin
    for i in actual_code_length-1 downto 0 loop
      code(index + i) := character'val(to_integer(value and BYTE_MASK));
      value := value srl code_length;
    end loop;
    index := index + actual_code_length;
  end procedure;

  procedure decode_raw_bit_array(constant code : in code_t; variable index : inout code_index_t; variable result : out bit_array) is
    constant actual_code_length : natural := code_length_raw_bit_array(result'length);
    variable ret_val : bit_array(actual_code_length*code_length-1 downto 0);
  begin
    for i in 0 to actual_code_length-1 loop
      ret_val(
        (actual_code_length-i)*code_length-1 downto (actual_code_length-i-1)*code_length
      ) := bit_array(ieee.numeric_bit.to_unsigned(character'pos(code(index + i)), code_length));
    end loop;
    result := ret_val(result'length-1 downto 0);
    index := index + actual_code_length;
  end procedure;

  -----------------------------------------------------------------------------
  -- bit_array
  -----------------------------------------------------------------------------
  procedure encode_bit_array(constant data : in bit_array; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    encode_range(data'left, data'right, data'ascending, index, code);
    encode_raw_bit_array(data, index, code);
  end procedure;

  procedure decode_bit_array(constant code : in code_t; variable index : inout code_index_t; variable result : out bit_array) is
  begin
    index := index + code_length_range_type;
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
  -- raw_std_ulogic_array
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

  procedure encode_raw_std_ulogic_array(constant data : in std_ulogic_array; variable index : inout code_index_t; variable code : inout code_t) is
    constant actual_code_length : natural := code_length_raw_std_ulogic_array(data'length);
    variable i    : integer := data'left;
    variable byte : natural;
    constant idx_increment : integer := idx_increment(data'ascending);
    constant factor : positive := 2**bits_length_std_ulogic;
  begin
    -- One std_ulogic can represent length_std_ulogic=9 value: it needs bits_length_std_ulogic=4 bits to store it.
    -- In a character (code_length=8 bits), we can store code_length/bits_length_std_ulogic=2 std_ulogic elements.
    for idx in 0 to actual_code_length-1 loop
      -- Encode the first std_ulogic
      byte := std_ulogic'pos(data(i));
      -- Encode the second std_ulogic (if not at the end of the std_ulogic_array)
      if i /= data'right then
        i := i + idx_increment;
        byte := byte + std_ulogic'pos(data(i)) * factor;
        i := i + idx_increment;
      end if;
      -- Convert into a character and stores it into the string
      code(index + idx) := character'val(byte);
    end loop;
    index := index + actual_code_length;
  end procedure;

  procedure decode_raw_std_ulogic_array(constant code : in code_t; variable index : inout code_index_t; variable result : out std_ulogic_array) is
    constant actual_code_length : natural := code_length_raw_std_ulogic_array(result'length);
    variable i : integer := result'left;
    variable upper_nibble : natural;
    constant idx_increment : integer := idx_increment(result'ascending);
    constant factor : positive := 2**bits_length_std_ulogic;
  begin
    for idx in 0 to actual_code_length-1 loop
      -- Decode the second std_ulogic
        if i /= result'right then
          upper_nibble := character'pos(code(index + idx)) / factor;
          result(i + idx_increment) := std_ulogic'val(upper_nibble);
        else
          upper_nibble := 0;
        end if;
      -- Decode the first std_ulogic
      result(i) := std_ulogic'val(character'pos(code(index + idx)) - upper_nibble*factor);
      i := i + 2*idx_increment;
    end loop;
    index := index + actual_code_length;
  end procedure;

  -----------------------------------------------------------------------------
  -- std_ulogic_array
  -----------------------------------------------------------------------------
  procedure encode_std_ulogic_array(constant data : in std_ulogic_array; variable index : inout code_index_t; variable code : inout code_t) is
  begin
    encode_range(data'left, data'right, data'ascending, index, code);
    encode_raw_std_ulogic_array(data, index, code);
  end procedure;

  procedure decode_std_ulogic_array(constant code : in code_t; variable index : inout code_index_t; variable result : out std_ulogic_array) is
  begin
    index := index + code_length_range_type;
    decode_raw_std_ulogic_array(code, index, result);
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
  -- Encode and Decode functions of composite types (arrays) with their range encoded
  --===========================================================================

  -----------------------------------------------------------------------------
  -- raw_bit_array
  -----------------------------------------------------------------------------
  function encode_raw_bit_array(data : bit_array) return code_t is
    variable ret_val : code_t(1 to code_length_raw_bit_array(data'length));
    variable index : code_index_t := ret_val'left;
  begin
    encode_raw_bit_array(data, index, ret_val);
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
    return decode_raw_bit_array(code, code'length*code_length);
  end function;

  -----------------------------------------------------------------------------
  -- raw_std_ulogic_array
  -----------------------------------------------------------------------------
  function encode_raw_std_ulogic_array(data : std_ulogic_array) return code_t is
    variable ret_val : code_t(1 to code_length_raw_std_ulogic_array(data'length));
    variable index : code_index_t := ret_val'left;
  begin
    encode_raw_std_ulogic_array(data, index, ret_val);
    return ret_val;
  end function;

  function decode_raw_std_ulogic_array(code : code_t; length : positive) return std_ulogic_array is
    variable ret_val : std_ulogic_array(length-1 downto 0);
    variable index : code_index_t := code'left;
  begin
    decode_raw_std_ulogic_array(code, index, ret_val);
    return ret_val;
  end function;

  function decode_raw_std_ulogic_array(code : code_t) return std_ulogic_array is
  begin
    return decode_raw_std_ulogic_array(code, code'length*code_length);
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
