{ ==============================================================================

  REGISTERS FRAME BASE CLASS


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

unit uRegistersFrameBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls,
  //
  uMemoryMgr;

type
  TRegistersFrame = class(TFrame)
  protected
    fMemoryRef: TMemory;
    fStackAssigned: integer;
  public
    procedure Initialise; virtual; abstract;
    procedure Refresh;    virtual; abstract;
    //
    property MemoryRef: TMemory read fMemoryRef write fMemoryRef;
    property StackAssigned: integer read fStackAssigned write fStackAssigned;
  end;


implementation

{$R *.lfm}


end.

