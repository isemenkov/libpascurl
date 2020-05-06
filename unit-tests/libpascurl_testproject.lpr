program libpascurl_testproject;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, timeintervaltestcase, datasizetestcase,
  pascurl, http, result, timeinterval, datasize, errorstack;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

