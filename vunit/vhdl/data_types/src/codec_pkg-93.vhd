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

library work;
use work.common_pkg.all;


-------------------------------------------------------------------------------
-- Package declaration
-------------------------------------------------------------------------------
package codec_v1993_pkg is

  -- This packages enables the user to encode any predefined type into a unique type.
  -- This unique type is a 'string' (array of 'character').
  -- The functionality can be used to build a queue capable of storing different
  -- types in it (see the VUnit 'queue' package)
  alias code_t is string;


  --===========================================================================
  -- API for the CASUAL USERS
  --===========================================================================
  -- All data going through the encoding process becomes a string: it is
  -- basically becomes a sequence of bytes without any overhead for type
  -- information. The 'codec' package doesn’t know if four bytes represents an
  -- integer, four characters or something else. The interpretation of these
  -- bytes takes place when the user decodes the data using a type specific
  -- 'decode' function.

  -----------------------------------------------------------------------------
  -- Encode and decode functions of predefined enumerated types
  -----------------------------------------------------------------------------

  function encode_boolean(data : boolean) return code_t;
  function decode_boolean(code : code_t) return boolean;
  alias encode is encode_boolean[boolean return code_t];
  alias decode is decode_boolean[code_t return boolean];

  function encode_character(data : character) return code_t;
  function decode_character(code : code_t) return character;
  alias encode is encode_character[character return code_t];
  alias decode is decode_character[code_t return character];

  function encode_bit(data : bit) return code_t;
  function decode_bit(code : code_t) return bit;
  alias encode is encode_bit[bit return code_t];
  alias decode is decode_bit[code_t return bit];

  function encode_std_ulogic(data : std_ulogic) return code_t;
  function decode_std_ulogic(code : code_t) return std_ulogic;
  alias encode is encode_std_ulogic[std_ulogic return code_t];
  alias decode is decode_std_ulogic[code_t return std_ulogic];

  function encode_severity_level(data : severity_level) return code_t;
  function decode_severity_level(code : code_t) return severity_level;
  alias encode is encode_severity_level[severity_level return code_t];
  alias decode is decode_severity_level[code_t return severity_level];

  function encode_file_open_kind(data : file_open_kind) return code_t;
  function decode_file_open_kind(code : code_t) return file_open_kind;
  alias encode is encode_file_open_kind[file_open_kind return code_t];
  alias decode is decode_file_open_kind[code_t return file_open_kind];

  function encode_file_open_status(data : file_open_status) return code_t;
  function decode_file_open_status(code : code_t) return file_open_status;
  alias encode is encode_file_open_status[file_open_status return code_t];
  alias decode is decode_file_open_status[code_t return file_open_status];


  -----------------------------------------------------------------------------
  -- Encode and decode functions of predefined scalar types
  -----------------------------------------------------------------------------

  function encode_integer(data : integer) return code_t;
  function decode_integer(code : code_t) return integer;
  alias encode is encode_integer[integer return code_t];
  alias decode is decode_integer[code_t return integer];

  function encode_real(data : real) return code_t;
  function decode_real(code : code_t) return real;
  alias encode is encode_real[real return code_t];
  alias decode is decode_real[code_t return real];

  function encode_time(data : time) return code_t;
  function decode_time(code : code_t) return time;
  alias encode is encode_time[time return code_t];
  alias decode is decode_time[code_t return time];


  -----------------------------------------------------------------------------
  -- Encode and decode functions of predefined composite types (records)
  -----------------------------------------------------------------------------

  function encode_complex(data : complex) return code_t;
  function decode_complex(code : code_t) return complex;
  alias encode is encode_complex[complex return code_t];
  alias decode is decode_complex[code_t return complex];

  function encode_complex_polar(data : complex_polar) return code_t;
  function decode_complex_polar(code : code_t) return complex_polar;
  alias encode is encode_complex_polar[complex_polar return code_t];
  alias decode is decode_complex_polar[code_t return complex_polar];


  -----------------------------------------------------------------------------
  -- Encode and decode functions of predefined composite types (arrays)
  -----------------------------------------------------------------------------

  function encode_string(data : string) return code_t;
  function decode_string(code : code_t) return string;
  alias encode is encode_string[string return code_t];
  alias decode is decode_string[code_t return string];

  function encode_bit_vector(data : bit_vector) return code_t;
  function decode_bit_vector(code : code_t) return bit_vector;
  alias encode is encode_bit_vector[bit_vector return code_t];
  alias decode is decode_bit_vector[code_t return bit_vector];

  function encode_numeric_bit_unsigned(data : ieee.numeric_bit.unsigned) return code_t;
  function decode_numeric_bit_unsigned(code : code_t) return ieee.numeric_bit.unsigned;
  alias encode is encode_numeric_bit_unsigned[ieee.numeric_bit.unsigned return code_t];
  alias decode is decode_numeric_bit_unsigned[code_t return ieee.numeric_bit.unsigned];

  function encode_numeric_bit_signed(data : ieee.numeric_bit.signed) return code_t;
  function decode_numeric_bit_signed(code : code_t) return ieee.numeric_bit.signed;
  alias encode is encode_numeric_bit_signed[ieee.numeric_bit.signed return code_t];
  alias decode is decode_numeric_bit_signed[code_t return ieee.numeric_bit.signed];

  function encode_std_ulogic_vector(data : std_ulogic_vector) return code_t;
  function decode_std_ulogic_vector(code : code_t) return std_ulogic_vector;
  alias encode is encode_std_ulogic_vector[std_ulogic_vector return code_t];
  alias decode is decode_std_ulogic_vector[code_t return std_ulogic_vector];

  function encode_numeric_std_unsigned(data : ieee.numeric_std.unresolved_unsigned) return code_t;
  function decode_numeric_std_unsigned(code : code_t) return ieee.numeric_std.unresolved_unsigned;
  alias encode is encode_numeric_std_unsigned[ieee.numeric_std.unresolved_unsigned return code_t];
  alias decode is decode_numeric_std_unsigned[code_t return ieee.numeric_std.unresolved_unsigned];

  function encode_numeric_std_signed(data : ieee.numeric_std.signed) return code_t;
  function decode_numeric_std_signed(code : code_t) return ieee.numeric_std.signed;
  alias encode is encode_numeric_std_signed[ieee.numeric_std.signed return code_t];
  alias decode is decode_numeric_std_signed[code_t return ieee.numeric_std.signed];



  --===========================================================================
  -- API for the ADVANCED USERS
  --===========================================================================

  -----------------------------------------------------------------------------
  -- Base types and constants describing how the encoding is performed
  -----------------------------------------------------------------------------

  -- A character (string of length 1) stores value on 8 bits
  constant CODE_LENGTH : positive := 8;
  -- A character (string of length 1) can store up to 2**8 = 256 value
  constant CODE_NB_VALUES : positive := 2**CODE_LENGTH;


  -----------------------------------------------------------------------------
  -- VUnit defined types
  -----------------------------------------------------------------------------

  -- The ieee.std_ulogic_vector is defined with a natural range.
  -- If you need to encode an array of ieee.std_ulogic (or an array of any subtype
  -- of ieee.std_ulogic) with an integer range, you can use the type 'std_ulogic_array'
  type bit_array is array(integer range <>) of bit;

  function encode_bit_array(data : bit_array) return code_t;
  function decode_bit_array(code : code_t) return bit_array;
  alias encode is encode_bit_array[bit_array return code_t];
  alias decode is decode_bit_array[code_t return bit_array];

  -- The std.bit_vector is defined with a natural range.
  -- If you need to encode an array of std.bit (or an array of any subtype
  -- of std.bit) with an integer range, you can use the type 'bit_array'
  type std_ulogic_array is array(integer range <>) of std_ulogic;

  function encode_std_ulogic_array(data : std_ulogic_array) return code_t;
  function decode_std_ulogic_array(code : code_t) return std_ulogic_array;
  alias encode is encode_std_ulogic_array[std_ulogic_array return code_t];
  alias decode is decode_std_ulogic_array[code_t return std_ulogic_array];


  -----------------------------------------------------------------------------
  -- Encoding length for each types
  -----------------------------------------------------------------------------
  -- If you need to retrieve the length of the encoded data whitout,
  -- encoding it, you can use these constants/functions:

  -- Encoding length of predefined enumerated types:
  -- The formulation "type'pos(type'right) + 1" gives the number of element of the enumerated type
  constant LENGTH_BOOLEAN          : positive := boolean'pos(boolean'right) + 1;
  constant LENGTH_CHARACTER        : positive := character'pos(character'right) + 1;
  constant LENGTH_BIT              : positive := bit'pos(bit'right) + 1;
  constant LENGTH_STD_ULOGIC       : positive := std_ulogic'pos(std_ulogic'right) + 1;
  constant LENGTH_SEVERITY_LEVEL   : positive := severity_level'pos(severity_level'right) + 1;
  constant LENGTH_FILE_OPEN_KIND   : positive := file_open_kind'pos(file_open_kind'right) + 1;
  constant LENGTH_FILE_OPEN_STATUS : positive := file_open_status'pos(file_open_status'right) + 1;

  -- Encoding length of predefined enumerated types:
  constant CODE_LENGTH_BOOLEAN          : positive := ceil_div(LENGTH_BOOLEAN, CODE_LENGTH);
  constant CODE_LENGTH_CHARACTER        : positive := ceil_div(LENGTH_CHARACTER, CODE_LENGTH);
  constant CODE_LENGTH_BIT              : positive := ceil_div(LENGTH_BIT, CODE_LENGTH);
  constant CODE_LENGTH_STD_ULOGIC       : positive := ceil_div(LENGTH_STD_ULOGIC, CODE_LENGTH);
  constant CODE_LENGTH_SEVERITY_LEVEL   : positive := ceil_div(LENGTH_SEVERITY_LEVEL, CODE_LENGTH);
  constant CODE_LENGTH_FILE_OPEN_KIND   : positive := ceil_div(LENGTH_FILE_OPEN_KIND, CODE_LENGTH);
  constant CODE_LENGTH_FILE_OPEN_STATUS : positive := ceil_div(LENGTH_FILE_OPEN_STATUS, CODE_LENGTH);

  -- Encoding length of predefined scalar types:
  constant CODE_LENGTH_INTEGER : positive := SIMULATOR_INTEGER_WIDTH/CODE_LENGTH;
  constant CODE_LENGTH_REAL    : positive := CODE_LENGTH_BOOLEAN + 3 * CODE_LENGTH_INTEGER;
  constant CODE_LENGTH_TIME    : positive := SIMULATOR_TIME_WIDTH/CODE_LENGTH;

  -- Encoding length of predefined composite types (records):
  constant CODE_LENGTH_COMPLEX       : positive := 2 * CODE_LENGTH_REAL;
  constant CODE_LENGTH_COMPLEX_POLAR : positive := 2 * CODE_LENGTH_REAL;

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
  constant CODE_LENGTH_RANGE_TYPE : positive := 2 * CODE_LENGTH_INTEGER + CODE_LENGTH_BOOLEAN;

  -- This type is used so that we can return an array with any integer range.
  -- It is not meant to carry any other information.
  type range_t is array(integer range <>) of bit;

  -- Encode and decode functions for range
  function encode_range(range_left : integer; range_right : integer; is_ascending : boolean) return code_t;
  function decode_range(code : code_t) return range_t;
  alias encode is encode_range[integer, integer, boolean return code_t];
  alias decode is decode_range[code_t return range_t];

  -- Null range constant
  constant ENCODED_NULL_RANGE : code_t;


  -----------------------------------------------------------------------------
  -- Encoding of bit_vector
  -----------------------------------------------------------------------------

  -- This function is used to return the encode value of a bit_array without its range encoded.
  -- To encode/decode bit_array, use encode_bit_array and decode_bit_array.
  function encode_raw_bit_array(data : bit_array) return code_t;
  function decode_raw_bit_array(code : code_t) return bit_array;
  function decode_raw_bit_array(code : code_t; length : positive) return bit_array;
  -- Note that this function does not have its alias 'encode' and
  -- 'decode' equivalent as they are homograph with the 'encode_bit_array'
  -- and 'decode_integer' functions

  -- These functions give you the length of the encoded array depending on the
  -- length of the array to encode
  function code_length_raw_bit_array(length : natural) return natural;


  -----------------------------------------------------------------------------
  -- Alternate decode procedures
  -----------------------------------------------------------------------------
  -- These are low level procedure to decode. They are not intented to be used by the casual user.
  -- These are intended for VUnit developers (and advanced users) to build decode
  -- functions of more complex types.
  -- The 'code' parameter is the encoded data.
  -- The 'result' parameter will be updated with the decoded data after the call to the procedure.
  -- The 'index' parameter indicates where inside the 'code' parameter the data to decode starts.
  -- The 'index' is update in order for the next decode procedure to decode the next
  -- encoded data in the 'code' parameter.
  -- The implementations on the 'decode_complex' is an example of that. We first decode
  -- the real part, the imaginary part.

  -- Index to track the position of an encoded element inside an instance of code_t
  subtype code_index_t is integer;

  -- Predefined enumerated types
  procedure decode_boolean(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out boolean
  );
  procedure decode_character(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out character
  );
  procedure decode_bit(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out bit
  );
  procedure decode_std_ulogic(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out std_ulogic
  );
  procedure decode_severity_level(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out severity_level
  );
  procedure decode_file_open_kind(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out file_open_kind
  );
  procedure decode_file_open_status(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out file_open_status
  );

  alias decode is decode_boolean[
    code_t, code_index_t, boolean
  ];
  alias decode is decode_character[
    code_t, code_index_t, character
  ];
  alias decode is decode_bit[
    code_t, code_index_t, bit
  ];
  alias decode is decode_std_ulogic[
    code_t, code_index_t, std_ulogic
  ];
  alias decode is decode_severity_level[
    code_t, code_index_t, severity_level
  ];
  alias decode is decode_file_open_kind[
    code_t, code_index_t, file_open_kind
  ];
  alias decode is decode_file_open_status[
    code_t, code_index_t, file_open_status
  ];

  -- Predefined scalar types
  procedure decode_integer(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out integer
  );
  procedure decode_real(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out real
  );
  procedure decode_time(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out time
  );

  alias decode is decode_integer[
    code_t, positive, integer
  ];
  alias decode is decode_real[
    code_t, positive, real
  ];
  alias decode is decode_time[
    code_t, positive, time
  ];

  -- Predefined composite types (records)
  procedure decode_complex(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out complex
  );
  procedure decode_complex_polar(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out complex_polar
  );

  alias decode is decode_complex[
    code_t, positive, complex
  ];
  alias decode is decode_complex_polar[
    code_t, positive, complex_polar
  ];

  -- Predefined composite types (arrays)
  procedure decode_string(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out string
  );
  procedure decode_raw_bit_array(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out bit_array
  );
  procedure decode_bit_array(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out bit_array
  );
  procedure decode_bit_vector(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out bit_vector
  );
  procedure decode_numeric_bit_unsigned(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out ieee.numeric_bit.unsigned
  );
  procedure decode_numeric_bit_signed(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out ieee.numeric_bit.signed
  );
  procedure decode_std_ulogic_array(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out std_ulogic_array
  );
  procedure decode_std_ulogic_vector(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out std_ulogic_vector
  );
  procedure decode_numeric_std_unsigned(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out ieee.numeric_std.unresolved_unsigned
  );
  procedure decode_numeric_std_signed(
    constant code : in code_t;
    variable index : inout code_index_t;
    variable result : out ieee.numeric_std.signed
  );

  alias decode is decode_string[
    code_t, positive, string
  ];
  alias decode is decode_bit_array[
    code_t, positive, bit_array
  ];
  alias decode is decode_bit_vector[
    code_t, positive, bit_vector
  ];
  alias decode is decode_numeric_bit_unsigned[
    code_t, positive, ieee.numeric_bit.unsigned
  ];
  alias decode is decode_numeric_bit_signed[
    code_t, positive, ieee.numeric_bit.signed
  ];
  alias decode is decode_std_ulogic_array[
    code_t, positive, std_ulogic_array
  ];
  alias decode is decode_std_ulogic_vector[
    code_t, positive, std_ulogic_vector
  ];
  alias decode is decode_numeric_std_unsigned[
    code_t, positive, ieee.numeric_std.unresolved_unsigned
  ];
  alias decode is decode_numeric_std_signed[
    code_t, positive, ieee.numeric_std.signed
  ];


  -----------------------------------------------------------------------------
  -- Deprecated functions. Maintained for backward compatibility
  -----------------------------------------------------------------------------

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
  -- Use the 'decode_range' function instead.
  function get_range(code : code_t) return range_t;

  -- This function is deprecated.
  -- Use the 'encode_raw_bit_array' function instead.
  function to_byte_array(value : bit_vector) return code_t;

  -- This function is deprecated.
  -- Use the 'decode_raw_bit_array' function instead.
  function from_byte_array(byte_array : code_t) return bit_vector;

end package;
