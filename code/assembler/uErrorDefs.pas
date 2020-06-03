{ ==============================================================================

  ERROR TEXTS FOR ASSEMBLER


  LICENSE:

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or any
    later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

  =============================================================================}

unit uErrorDefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  // uAssembler
  ADDR_MODE_NOT_RECOGNISED = 'Address mode [%s] not recognised';
  BRANCH_TOO_FAR           = 'Branch too far';
  CANNOT_FIND_INCLUDE_FILE = 'Cannot find include file [%s]';
  ELSE_WITHOUT_IF          = 'ELSE without starting IF';
  ENDIF_WITHOUT_IF         = 'ENDIF without starting IF';
  EXPECTED_NOT_FOUND       = '[%s] expected but [%s] found';
  INSTR_NOT_RECOGNISED     = 'Instruction [%s] not recognised';
  INSTR_EXPECTED           = 'Instruction expected, got [%s]';
  LABEL_MISSING            = 'Label is required for this instruction';
  MEM_MODE_NOT_RECOGNISED  = 'Memory mode [%s] not recognised';
  OPERAND_NOT_FOUND        = 'Operand expected, not found';
  PHASING_ERROR            = 'Symbol values differ between passes ($%.4x,$%.4x), check addressing mode';
  SYMBOL_NOT_DEFINED       = 'Symbol [%s] not defined';
  MACRO_NAME_MISSING       = 'Macro name missing';
  MACRO_BAD_INSTRUCTION    = 'This instruction not permitted in a macro definition';

  PASS_2_ABORTED           = 'Second pass aborted due to %d %s in first pass';

  // uParser
  PAR_UNTERMINATED_STRING  = 'Unterminated String';
  PAR_ILLEGAL_NUMBER       = 'Illegal number format';

  // uReadWriteHex
  RWH_NOT_SUPPORTED        = 'Record type %d not yet supported';
  RWH_NOT_VALID            = 'Record type %d not valid';
  RWH_INVALID_HEX          = '[%s] is not a valid hex number';
  RWH_MISSING_COLON        = 'Missing colon at start of line';


implementation


end.

