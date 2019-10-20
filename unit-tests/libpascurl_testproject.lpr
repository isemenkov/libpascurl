program libpascurl_testproject;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, timeintervaltestcase, datasizetestcase,
  pascurl;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

