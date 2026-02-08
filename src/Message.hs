module Message where
import Item (Item)

data Message = NewData [Item] | NextPage | LastPage | NewSearch | EnterSite String | Error String deriving Show
