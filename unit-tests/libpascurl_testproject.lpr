program libpascurl_testproject;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, errorstacktestcase, libpascurl,
  optionaltestcase, resulttestcase, timeintervaltestcase, datasizetestcase,
  stringlisttestcase;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

