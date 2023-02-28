unit itm_api;

{ Declarations of imported procedures from the ITM DLL engine }

interface

uses Uutils;


// Integrate with ITM, written in Fortran and C.  Most of the C++/Delphi
// integration comes courtesy of Rudy's Delphi Cornder
// (http://rvelthuis.de/articles/articles-cppobjs.html).

function itm_dll_init(
      F1: PAnsiChar;
      F2: PAnsiChar;
      F3: PAnsiChar;
      F4: PAnsiChar;
      F5: PAnsiChar;
//      F6: PAnsiChar;
//      F7: PAnsiChar;
      F1len: longint;
      F2len: longint;
      F3len: longint;
      F4len: longint;
      F5len: longint): Integer; stdcall;
//      F6len: longint;
//      F7len: longint
function itm_exec_step(var Time: Double): Integer; stdcall;
function itm_get_mass_bal_error(var continuityError: Double): Integer; stdcall;
function itm_dll_end(): Integer; stdcall;


implementation


function itm_dll_init; external 'ITM_DLL.DLL';
function itm_exec_step; external 'ITM_DLL.DLL';
function itm_get_mass_bal_error; external 'ITM_DLL.DLL';
function itm_dll_end; external 'ITM_DLL.DLL';


end.

