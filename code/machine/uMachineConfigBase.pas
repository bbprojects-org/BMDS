{ ==============================================================================

  MACHINE CONFIGURATION BASE CLASS

    This is the base frame that is inherited by all configuration frames


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

unit uMachineConfigBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls;

type
  TMachineConfigFrame = class(TFrame)
  private
  public
    procedure Init; virtual; abstract;
    destructor Destroy; override;
  end;


implementation

{$R *.lfm}

{ TMachineConfigFrame }

destructor TMachineConfigFrame.Destroy;
begin
  //
  inherited Destroy;
end;


end.

