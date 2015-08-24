unit DragDropTimeOfDay;

interface

uses
  Graphics;

type
  // TTimeOfDay is the structure which is transferred from the drop source to
  // the drop target.
  TTimeOfDay = record
    hours,
    minutes,
    seconds,
    milliseconds: word;
    color: TColor;
  end;

const
  // Name of our custom clipboard format.
  sTimeOfDayName = 'TimeOfDay';

implementation

end.
