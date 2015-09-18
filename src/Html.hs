module Html
    (displayHtml,
     generateHtml,
     Element(..)
    ) where

type AttributeName = String
type AttributeValue = String
data Attribute = Attribute AttributeName AttributeValue
type Attributes = [Attribute]

type TagName = String
data Element = Text String
             | Tag TagName Attributes Html
             | SelfClosingTag TagName Attributes Html

type Html = [Element]

type Root = Element
type Head = [Element]
type Body = [Element]

example :: IO ()
example = putStrLn $ displayElement $ generateHtml [] [Tag "div" [Attribute "class" "row"]
                                                      [Tag "div" [Attribute "class" "col-xs-6"]
                                                         [Tag "p" [Attribute "class" "header-text"]
                                                         [Text "Welcome"]],
                                                       Tag "div" [Attribute "class" "col-xs-6"]
                                                         [Tag "img" [Attribute "class" "header-picture"] []]]]

displayHtml :: Root -> String
displayHtml = displayElement

generateHtml :: Head -> Body -> Root
generateHtml header body = SelfClosingTag "!DOCTYPE" []
                          [Tag "html" []
                             [Tag "head" [] header,
                              Tag "body" [] body]]

displayElement :: Element -> String
displayElement (Text x) = x
displayElement x = "<" ++ getName x ++ displayAttributes (getAttributes x) ++ ">" ++ displayElements (getHtml x)
                ++ case x of
                     Tag _ _ _ -> "</" ++ getName x ++ ">"
                     _         -> ""

getName :: Element -> String
getName (Tag tagName _ _) = tagName
getName (SelfClosingTag tagName _ _) = tagName
getName (Text _) = error "Can't get the name of Text"

getAttributes :: Element -> [Attribute]
getAttributes (Tag _ attributes _) = attributes
getAttributes (SelfClosingTag _ attributes _) = attributes
getAttributes (Text _) = error "Can't get the attributes of Text"

getHtml :: Element -> Html
getHtml (Tag _ _ html) = html
getHtml (SelfClosingTag _ _ html) = html
getHtml (Text _) = error "Can't get the attributes of Text"

displayAttributes :: Attributes -> String
displayAttributes = foldl foldFn ""
    where foldFn acc (Attribute attributeName attributeValue) = " " ++ attributeName ++
                                                                  (if attributeValue /= "" then "=\"" ++ attributeValue ++ "\"" else "") ++
                                                                  acc

displayElements :: Html -> String
displayElements elements = foldl (++) "" (map displayElement elements)
