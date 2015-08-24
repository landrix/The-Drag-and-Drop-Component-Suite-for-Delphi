object DataModuleDropHandler: TDataModuleDropHandler
  OldCreateOrder = False
  Height = 150
  Width = 215
  object DropHandler1: TDropHandler
    DragTypes = [dtCopy]
    OnDrop = DropHandler1Drop
    OptimizedMove = True
    Left = 36
    Top = 16
  end
end
