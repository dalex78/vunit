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

  -----------------------------------------------------------------------------
  -- Base types and functions
  -----------------------------------------------------------------------------

  -- Target type containing the encoded values
  subtype code_t is character;
  -- type code_vec_t is array(integer range <>) of code_t;
  alias code_vec_t is string;
  -- Index of the first element in a code_vec_t instance
  constant CODE_VEC_LOW : positive := 1;
  -- Length (in bits) of one instance of code_t
  constant CODE_LENGTH : positive := 8;
  -- Index to track the position of an encoded element inside an instance of code_vec_t
  subtype code_index_t is integer;


  -----------------------------------------------------------------------------
  -- Encoding length of predefined enumerated types
  -----------------------------------------------------------------------------
  -- The formulation "type'pos(type'right) + 1" gives the number of element of the enumerated type
  constant LENGTH_BOOLEAN          : positive := boolean'pos(boolean'right) + 1;
  constant LENGTH_CHARACTER        : positive := character'pos(character'right) + 1;
  constant LENGTH_BIT              : positive := bit'pos(bit'right) + 1;
  constant LENGTH_STD_ULOGIC       : positive := std_ulogic'pos(std_ulogic'right) + 1;
  constant LENGTH_SEVERITY_LEVEL   : positive := severity_level'pos(severity_level'right) + 1;
  constant LENGTH_FILE_OPEN_KIND   : positive := file_open_kind'pos(file_open_kind'right) + 1;
  constant LENGTH_FILE_OPEN_STATUS : positive := file_open_status'pos(file_open_status'right) + 1;


  -----------------------------------------------------------------------------
  -- Encoding length of predefined enumerated types
  -----------------------------------------------------------------------------
  constant CODE_LENGTH_BOOLEAN          : positive := ceil_div(LENGTH_BOOLEAN, CODE_LENGTH);
  constant CODE_LENGTH_CHARACTER        : positive := ceil_div(LENGTH_CHARACTER, CODE_LENGTH);
  constant CODE_LENGTH_BIT              : positive := ceil_div(LENGTH_BIT, CODE_LENGTH);
  constant CODE_LENGTH_STD_ULOGIC       : positive := ceil_div(LENGTH_STD_ULOGIC, CODE_LENGTH);
  constant CODE_LENGTH_SEVERITY_LEVEL   : positive := ceil_div(LENGTH_SEVERITY_LEVEL, CODE_LENGTH);
  constant CODE_LENGTH_FILE_OPEN_KIND   : positive := ceil_div(LENGTH_FILE_OPEN_KIND, CODE_LENGTH);
  constant CODE_LENGTH_FILE_OPEN_STATUS : positive := ceil_div(LENGTH_FILE_OPEN_STATUS, CODE_LENGTH);


  -----------------------------------------------------------------------------
  -- Encoding length of predefined scalar types
  -----------------------------------------------------------------------------
  constant CODE_LENGTH_INTEGER : positive := SIMULATOR_INTEGER_WIDTH/CODE_LENGTH;
  constant CODE_LENGTH_REAL    : positive := CODE_LENGTH_BOOLEAN + 3 * CODE_LENGTH_INTEGER;
  constant CODE_LENGTH_TIME    : positive := SIMULATOR_TIME_WIDTH/CODE_LENGTH;


  -----------------------------------------------------------------------------
  -- Encoding length of predefined composite types (records)
  -----------------------------------------------------------------------------
  constant CODE_LENGTH_COMPLEX : positive := 2 * CODE_LENGTH_REAL;
  constant CODE_LENGTH_COMPLEX_POLAR : positive := 2 * CODE_LENGTH_REAL;


  -----------------------------------------------------------------------------
  -- Encode and decode functions of predefined enumerated types
  -----------------------------------------------------------------------------

  function encode_boolean(constant data : boolean) return code_vec_t;
  function decode_boolean(constant code : code_vec_t) return boolean;
  alias encode is encode_boolean[boolean return code_vec_t];
  alias decode is decode_boolean[code_vec_t return boolean];

  function encode_character(constant data : character) return code_vec_t;
  function decode_character(constant code : code_vec_t) return character;
  alias encode is encode_character[character return code_vec_t];
  alias decode is decode_character[code_vec_t return character];

  function encode_bit(constant data : bit) return code_vec_t;
  function decode_bit(constant code : code_vec_t) return bit;
  alias encode is encode_bit[bit return code_vec_t];
  alias decode is decode_bit[code_vec_t return bit];

  function encode_std_ulogic(constant data : std_ulogic) return code_vec_t;
  function decode_std_ulogic(constant code : code_vec_t) return std_ulogic;
  alias encode is encode_std_ulogic[std_ulogic return code_vec_t];
  alias decode is decode_std_ulogic[code_vec_t return std_ulogic];

  function encode_severity_level(constant data : severity_level) return code_vec_t;
  function decode_severity_level(constant code : code_vec_t) return severity_level;
  alias encode is encode_severity_level[severity_level return code_vec_t];
  alias decode is decode_severity_level[code_vec_t return severity_level];

  function encode_file_open_kind(constant data : file_open_kind) return code_vec_t;
  function decode_file_open_kind(constant code : code_vec_t) return file_open_kind;
  alias encode is encode_file_open_kind[file_open_kind return code_vec_t];
  alias decode is decode_file_open_kind[code_vec_t return file_open_kind];

  function encode_file_open_status(constant data : file_open_status) return code_vec_t;
  function decode_file_open_status(constant code : code_vec_t) return file_open_status;
  alias encode is encode_file_open_status[file_open_status return code_vec_t];
  alias decode is decode_file_open_status[code_vec_t return file_open_status];


  -----------------------------------------------------------------------------
  -- Encode and decode functions of predefined scalar types
  -----------------------------------------------------------------------------

  function encode_integer(constant data : integer) return code_vec_t;
  function decode_integer(constant code : code_vec_t) return integer;
  alias encode is encode_integer[integer return code_vec_t];
  alias decode is decode_integer[code_vec_t return integer];

  function encode_real(constant data : real) return code_vec_t;
  function decode_real(constant code : code_vec_t) return real;
  alias encode is encode_real[real return code_vec_t];
  alias decode is decode_real[code_vec_t return real];

  function encode_time(constant data : time) return code_vec_t;
  function decode_time(constant code : code_vec_t) return time;
  alias encode is encode_time[time return code_vec_t];
  alias decode is decode_time[code_vec_t return time];


  -----------------------------------------------------------------------------
  -- Encode and decode functions of predefined composite types (records)
  -----------------------------------------------------------------------------

  function encode_complex(constant data : complex) return code_vec_t;
  function decode_complex(constant code : code_vec_t) return complex;
  alias encode is encode_complex[complex return code_vec_t];
  alias decode is decode_complex[code_vec_t return complex];

  function encode_complex_polar(constant data : complex_polar) return code_vec_t;
  function decode_complex_polar(constant code : code_vec_t) return complex_polar;
  alias encode is encode_complex_polar[complex_polar return code_vec_t];
  alias decode is decode_complex_polar[code_vec_t return complex_polar];


  -----------------------------------------------------------------------------
  -- Encoding of predefined composite types (arrays)
  -----------------------------------------------------------------------------

  -- Two things need to be extracted from an array and encoded:
  --  * The range of the array
  --  * The data inside the array
  -- The range encoding is performed by 'encode_range' and 'decode_range'.
  -- Each predefined array types are transformed into a new type: 'std_ulogic_array'
  -- (at the exception of the string type)
  -- This enables to perform the encoding/decoding for this single type. All the
  -- other predefined array type are then converted into this generic array.
  -- Note: this array has a different range than std_ulogic_vector
  type std_ulogic_array is array(integer range <>) of std_ulogic;

  -- A range is constituted of two bounds (an left bound and a right bound)
  -- We also need to store the ascending/descending attribute to when the
  -- range is of lenght 1 or when the range is null
  constant CODE_LENGTH_RANGE : positive := 2 * CODE_LENGTH_INTEGER + CODE_LENGTH_BOOLEAN;


  -----------------------------------------------------------------------------
  -- Encode and decode functions for range
  -----------------------------------------------------------------------------

  -- This type is used so that we can return an array with any integer range.
  -- It is not meant to carry any other information.
  type range_t is array(integer range <>) of bit;

  function encode_range(
    constant range_left   : integer;
    constant range_right  : integer;
    constant is_ascending : boolean
  ) return code_vec_t;

  -- Note that, contrary to the above decode functions, this one has an extra parameters 'index'
  -- For the casual user, the parameter can be left to its default value
  -- This parameter is necessary as we are dealing with unconstrained array TODO
  function decode_range(code : code_vec_t; index : code_index_t := 0) return range_t;

  alias encode is encode_range[integer, integer, boolean return code_vec_t];
  alias decode is decode_range[code_vec_t, code_index_t return range_t];


  -----------------------------------------------------------------------------
  -- Encode and decode functions of predefined composite types (arrays)
  -----------------------------------------------------------------------------

  -- function encode_string(is_ascending : string) return code_vec_t;
  -- function decode_string(code : code_vec_t; index : code_index_t := 0) return string;
  -- alias encode is encode_string[string return code_vec_t];
  -- alias decode is decode_string[code_vec_t, code_index_t return string];

  -- function encode_std_ulogic_array(is_ascending : std_ulogic_array) return code_vec_t;
  -- function decode_std_ulogic_array(code : code_vec_t; index : code_index_t := 0) return std_ulogic_array;
  -- alias encode is encode_std_ulogic_array[std_ulogic_array return code_vec_t];
  -- alias decode is decode_std_ulogic_array[code_vec_t, code_index_t return std_ulogic_array];

  -- function encode_std_ulogic_vector(is_ascending : std_ulogic_vector) return code_vec_t;
  -- function decode_std_ulogic_vector(code : code_vec_t; index : code_index_t := 0) return std_ulogic_vector;
  -- alias encode is encode_std_ulogic_vector[std_ulogic_vector return code_vec_t];
  -- alias decode is decode_std_ulogic_vector[code_vec_t, code_index_t return std_ulogic_vector];

  -- function encode_bit_vector(is_ascending : bit_vector) return code_vec_t;
  -- function decode_bit_vector(code : code_vec_t; index : code_index_t := 0) return bit_vector;
  -- alias encode is encode_bit_vector[bit_vector return code_vec_t];
  -- alias decode is decode_bit_vector[code_vec_t, code_index_t return bit_vector];

  -- function encode_numeric_bit_unsigned(is_ascending : ieee.numeric_bit.unsigned) return code_vec_t;
  -- function decode_numeric_bit_unsigned(code : code_vec_t; index : code_index_t := 0) return ieee.numeric_bit.unsigned;
  -- alias encode is encode_numeric_bit_unsigned[ieee.numeric_bit.unsigned return code_vec_t];
  -- alias decode is decode_numeric_bit_unsigned[code_vec_t, code_index_t return ieee.numeric_bit.unsigned];

  -- function encode_numeric_bit_signed(is_ascending : ieee.numeric_bit.signed) return code_vec_t;
  -- function decode_numeric_bit_signed(code : code_vec_t; index : code_index_t := 0) return ieee.numeric_bit.signed;
  -- alias encode is encode_numeric_bit_signed[ieee.numeric_bit.signed return code_vec_t];
  -- alias decode is decode_numeric_bit_signed[code_vec_t, code_index_t return ieee.numeric_bit.signed];

  -- function encode_numeric_std_unsigned(is_ascending : ieee.numeric_std.unsigned) return code_vec_t;
  -- function decode_numeric_std_unsigned(code : code_vec_t; index : code_index_t := 0) return ieee.numeric_std.unsigned;
  -- alias encode is encode_numeric_std_unsigned[ieee.numeric_std.unsigned return code_vec_t];
  -- alias decode is decode_numeric_std_unsigned[code_vec_t, code_index_t return ieee.numeric_std.unsigned];

  -- function encode_numeric_std_signed(is_ascending : ieee.numeric_std.signed) return code_vec_t;
  -- function decode_numeric_std_signed(code : code_vec_t; index : code_index_t := 0) return ieee.numeric_std.signed;
  -- alias encode is encode_numeric_std_signed[ieee.numeric_std.signed return code_vec_t];
  -- alias decode is decode_numeric_std_signed[code_vec_t, code_index_t return ieee.numeric_std.signed];


  -----------------------------------------------------------------------------
  -- Alternate decode procedures
  -----------------------------------------------------------------------------
  -- TODO add description

  -- Predefined enumerated types
  procedure decode_boolean(
    constant code : code_vec_t; variable index : inout code_index_t; variable result : out boolean
  );
  procedure decode_character(
    constant code : code_vec_t; variable index : inout code_index_t; variable result : out character
  );
  procedure decode_bit(
    constant code : code_vec_t; variable index : inout code_index_t; variable result : out bit
  );
  procedure decode_std_ulogic(
    constant code : code_vec_t; variable index : inout code_index_t; variable result : out std_ulogic
  );
  procedure decode_severity_level(
    constant code : code_vec_t; variable index : inout code_index_t; variable result : out severity_level
  );
  procedure decode_file_open_kind(
    constant code : code_vec_t; variable index : inout code_index_t; variable result : out file_open_kind
  );
  procedure decode_file_open_status(
    constant code : code_vec_t; variable index : inout code_index_t; variable result : out file_open_status
  );

  alias decode is decode_boolean[
    code_vec_t, code_index_t, boolean
  ];
  alias decode is decode_character[
    code_vec_t, code_index_t, character
  ];
  alias decode is decode_bit[
    code_vec_t, code_index_t, bit
  ];
  alias decode is decode_std_ulogic[
    code_vec_t, code_index_t, std_ulogic
  ];
  alias decode is decode_severity_level[
    code_vec_t, code_index_t, severity_level
  ];
  alias decode is decode_file_open_kind[
    code_vec_t, code_index_t, file_open_kind
  ];
  alias decode is decode_file_open_status[
    code_vec_t, code_index_t, file_open_status
  ];


  -- Predefined scalar types
  procedure decode_integer(
    constant code : code_vec_t; variable index : inout code_index_t; variable result : out integer
  );
  procedure decode_real(
    constant code : code_vec_t; variable index : inout code_index_t; variable result : out real
  );
  procedure decode_time(
    constant code : code_vec_t; variable index : inout code_index_t; variable result : out time
  );

  alias decode is decode_integer[
    code_vec_t, positive, integer
  ];
  alias decode is decode_real[
    code_vec_t, positive, real
  ];
  alias decode is decode_time[
    code_vec_t, positive, time
  ];


  -- Predefined composite types (records)
  procedure decode_complex(
    constant code : code_vec_t; variable index : inout code_index_t; variable result : out complex
  );
  procedure decode_complex_polar(
    constant code : code_vec_t; variable index : inout code_index_t; variable result : out complex_polar
  );

  alias decode is decode_complex[
    code_vec_t, positive, complex
  ];
  alias decode is decode_complex_polar[
    code_vec_t, positive, complex_polar
  ];


  -- Range object
  procedure decode_range(
    constant code : code_vec_t; variable index : inout code_index_t; variable result : out range_t
  );
  alias decode is decode_range[
    code_vec_t, positive, range_t
  ];


  -- Predefined composite types (arrays)
  -- procedure decode_string(
  --   constant code : code_vec_t; variable index : inout code_index_t; variable result : out string
  -- );
  -- procedure decode_std_ulogic_array(
  --   constant code : code_vec_t; variable index : inout code_index_t; variable result : out std_ulogic_array
  -- );
  -- procedure decode_std_ulogic_vector(
  --   constant code : code_vec_t; variable index : inout code_index_t; variable result : out std_ulogic_vector
  -- );
  -- procedure decode_bit_vector(
  --   constant code : code_vec_t; variable index : inout code_index_t; variable result : out bit_vector
  -- );
  -- procedure decode_numeric_bit_unsigned(
  --   constant code : code_vec_t; variable index : inout code_index_t; variable result : out ieee.numeric_bit.unsigned
  -- );
  -- procedure decode_numeric_bit_signed(
  --   constant code : code_vec_t; variable index : inout code_index_t; variable result : out ieee.numeric_bit.signed
  -- );
  -- procedure decode_numeric_std_unsigned(
  --   constant code : code_vec_t; variable index : inout code_index_t; variable result : out ieee.numeric_std.unsigned
  -- );
  -- procedure decode_numeric_std_signed(
  --   constant code : code_vec_t; variable index : inout code_index_t; variable result : out ieee.numeric_std.signed
  -- );

  -- alias decode is decode_string[
  --   code_vec_t, positive, string
  -- ];
  -- alias decode is decode_std_ulogic_array[
  --   code_vec_t, positive, std_ulogic_array
  -- ];
  -- alias decode is decode_std_ulogic_vector[
  --   code_vec_t, positive, std_ulogic_vector
  -- ];
  -- alias decode is decode_bit_vector[
  --   code_vec_t, positive, bit_vector
  -- ];
  -- alias decode is decode_numeric_bit_unsigned[
  --   code_vec_t, positive, ieee.numeric_bit.unsigned
  -- ];
  -- alias decode is decode_numeric_bit_signed[
  --   code_vec_t, positive, ieee.numeric_bit.signed
  -- ];
  -- alias decode is decode_numeric_std_unsigned[
  --   code_vec_t, positive, ieee.numeric_std.unsigned
  -- ];
  -- alias decode is decode_numeric_std_signed[
  --   code_vec_t, positive, ieee.numeric_std.signed
  -- ];


  -----------------------------------------------------------------------------
  -- Deprecated functions. Maintained for backward compatibility
  -----------------------------------------------------------------------------

  function encode_array_header(
    constant range_left1   : code_vec_t;
    constant range_right1  : code_vec_t;
    constant is_ascending1 : code_vec_t;
    constant range_left2   : code_vec_t := "";
    constant range_right2  : code_vec_t := "";
    constant is_ascending2 : code_vec_t := "T"
  ) return code_vec_t;

  function get_range(
    constant code : code_vec_t
  ) return range_t;

  function to_byte_array(
    constant value : bit_vector
  ) return string;

  function from_byte_array(
    constant byte_array : string
  ) return bit_vector;

end package;
