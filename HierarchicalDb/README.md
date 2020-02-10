# Hierarchical Database
## Изготвил: Явор Петков, ф.н. 62187, СИ, 3-ти курс

### Въведение
Целта на проекта е да симулира йерархична база данни в интерактивен режим с нея. Всички данни от базата се пазят в db.json файл, който се намира в същата директория като програмата. Йерархичните данни представляват дървовидна структура, чиито корен е обекта "db". В базата от данни се поддържат колекции, чиито обекти имат еднаква структура (нещо като таблиците в релационните бази от данни). От тук следва, че данните трябва да са представени в дървовидна структура.
Данните са представени в следната структура.

### Ограничения
- Файлът, в който се държи базата от данни трябва да е в директорията, в която се намира.
- Толерансът за грешки е малък, при невалидни данни main функцията прекратява работа
- Няма `where` клауза при четенето на данните
- Изтриването и промяната на данните изискват задължителна `where` клауза.
###     

### Представяне на данните 
...
type Object = HashMap Text Value
type Array = Vector Value
data Value
  = Object Object
  | Array Array
  | String Text
  | Number Scientific
  | Bool Bool
  | Null
...
Чрез типа Value се постига дървовидната структура на данните. Root обектът има деца всички колекции в базата. Типът на данните е взет aeson (външен пакет за хаскел).

## Работа с данните
...
* readValue :: Value -> [T.Text] -> Value 
...
Служи за прочитане на property на обекта, подаден като първи аргумент. Бързодействието на тази операция зависи от това, колко дълбоко в йерархията е property-то.
Данните се конвертират до типа Value, чрез функцията `encode` от пакета Aeson. Данните се конвертират от типа Value до валиден JSON чрез функцията `decode` от пакета Aeson.

...
writeValue :: Value -> [T.Text] -> Value -> Value
...

С тази функция се обхожда дървовидната структура и се добавя/променя съществуващо property на данните. Като се обектът с променено пропърти.


## Селектиране на данните
След като работим с дървовидна структура, ще трябва да селектраме по удобен за нас начин данните. Синтаксисът е подобен на Xpath. Но условията, които се пишат за него се въвеждат по различен начин. Пътят до даденото пропърти, което се дава Xpath се обработва допълнително с фунцкията `path` подадена по-долу, след което листа, който се подава на функциите за обработка на данните.
...
path :: T.Text -> [T.Text]
...

## Основни CRUD операции
**readData** - четене на данните от базата
<per>
Read
Select schema
> people
Select properties (xpath like)

Index?
[{"person":{"age":21,"name":"Yavor"}}]
</pre>

**updateData** - четене на данните от базата, пример
<per>
Enter your operation 
>Update
Select schema
>people
Select property for where statement (xpath like syntax)
>person/name
Set value for property
>"yavor"
Select property for updating (xpath like syntax)
>person/name
Set value for property
>"Yavor"
Operation executed successfully
</pre>

**deleteData** - изтриване на елемент от колекция
</pre>
Enter your operation 
>Delete
Select schema
>people
Select property for where statement (xpath like syntax)
>person/name
Set value for property
>"ivan"
Operation executed successfully
</pre>

**addSchema** - добавяне на нова колекция, в която след това ще може да се обработват данни
<pre>
Enter your operation 
>AddSchema
Select schema
stars
Operation executed successfully
</pre>

**insertData** - добавяне на данни към колекция
<pre>
Enter your operation 
>Insert
Select schema
>students
Enter object in json notation
>{"student":{"name": "Ivan", "fn":11111, "address": {"country": "USA",  "street":"Some street name 1"}}}
Operation executed successfully
</pre>

## Външни пакети 
- `aeson` : изплозван за конвертиране на,текстът прочетен от db.JSON се парсва до обект от тип Maybe Value, и конвертиране на обект от тип Value до JSON формат
- `bytestring` за четене от и писане във файл и конвертирането до удобен формат
- `text` пакет, използван за превръщане на String към Text, използвани за ключове на имената на properties от тип Object.
- `vector` пакет, използван, защото масива от тип JSON се конвертира до vector, a list се обработва лесно. 
- unordered-containers 
