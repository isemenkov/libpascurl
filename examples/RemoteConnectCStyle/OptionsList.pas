unit OptionsList;

interface

uses
  container.arraylist, utils.functor, utils.pair;

type
  TOptionKeyValue = TPair<String, String>;
  TOptionKeyValueCompareFunctor =
    class(TDefaultCompareFunctor<TOptionKeyValue>)
  public
    function Call (AValue1, AValue2 : TOptionKeyValue) : Integer; override;
  end;

  TOptions = TArrayList<TOptionKeyValue, TOptionKeyValueCompareFunctor>;

implementation

function TOptionKeyValueCompareFunctor.Call (AValue1, AValue2 : TOptionKeyValue)
  : Integer;
begin
  if AValue1.First < AValue2.First then
    Result := -1
  else if AValue2.First < AValue1.First then
    Result := 1
  else
    Result := 0;
end;


end.
