erlang-LLSN
======

Это Erlang реализация кодирования и декодирования формата LLSN. Спецификация по
формату доступна здесь: http://allyst.org/opensource/llsn/ru/

Основы
------
Для кодирования данных необходимо описать структуру пакета следующим образом
1) пакет должен быть типа tuple
2) каждое поле этой структуры должно быть описано соответствующим типом (см. таблицу типов)
3) если описывается древовидная структура, то такой элемент должен быть описан типом ?LLSN_TYPE_POINTER


```Erlang
   Struct = {?LLSN_TYPE_NUMBER, ?LLSN_TYPE_STRING},
   Packet = {12345, "Hello World!"},
   Bin = llsn:encode(Packet, Struct).
```

Элемент массива описывается в виде tuple с двумя значениями - тип массива, тип элементов массива

```Erlang
   Struct = {
             ?LLSN_TYPE_NUMBER, ?LLSN_TYPE_STRING, 
             {?LLSN_TYPE_ARRAY, ?LLSN_TYPE_NUMBER}
            },
   Packet = {12345, "Hello World!", [1,2,3,4,5]},
   Bin = llsn:encode(Packet, Struct).
```

Если массив содержит NULL значения, то необходимо указать соответствующий тип массива, при этом
сами элементы должны быть представлены как 'null' значения. Чтобы изменить его, необходимо переопределить
макроопределение ?LLSN_NULL

```Erlang
   Struct = {
             ?LLSN_TYPE_NUMBER, ?LLSN_TYPE_STRING,
             {?LLSN_TYPE_ARRAYN, ?LLSN_TYPE_NUMBER}
            },
   Packet = {12345, "Hello World!", [1,2,null,null,5]},
   Bin = llsn:encode(Packet, Struct).
```

Структуры описываются также как и массивы в виде tuple, при этом второй элемент является типом list,
который содержит типы структур

```Erlang
   Struct = {
             ?LLSN_TYPE_NUMBER, ?LLSN_TYPE_STRING,
             {?LLSN_TYPE_ARRAYN, ?LLSN_TYPE_NUMBER},
             {?LLSN_TYPE_STRUCT,[?LLSN_TYPE_BOOL, ?LLSN_TYPE_DATE]}
            },
   Packet = {
             12345, "Hello World!",
             [1,2,null,null,5],
             {false, {{2015, 4, 15},{16, 56, 39, 678},{0,0}}}
            },
   Bin = llsn:encode(Packet, Struct).
```

Расширенные возможности
-----------------------
###Кодирование древовидных данных
В случае, если необходимо закодировать данные, имеющие древовидную структуру. В языке Си древовидные структуры описыаются через указатели. Например вот так

```C
typedef TreeStruct struct {
    int value,
    *TreeStruct child
}
```

В LLSN для этих целей предусмотрен соответствующий тип. ?LLSN_TYPE_POINTER. Это указатель на уже описаный тип ранее, в декларируемой структуре. Формат этого типа также является tuple, а координаты типа представлены в виде массива целых положительных чисел.
```Erlang
   Struct = {
             ?LLSN_TYPE_NUMBER, ?LLSN_TYPE_STRING,
             {?LLSN_TYPE_ARRAYN, ?LLSN_TYPE_NUMBER},
             {?LLSN_TYPE_STRUCT,[?LLSN_TYPE_BOOL, ?LLSN_TYPE_DATE]},
             {?LLSN_TYPE_STRUCT,[?LLSN_TYPE_NUMBER, {?LLSN_TYPE_POINTER,[3]}]}
            },
   Packet = {
             12345, "Hello World!",
             [1,2,null,null,5],
             {false, {{2015, 4, 15},{16, 56, 39, 678},{0,0}},
             {1,{2,{3,null}}}}
            },
   Bin = llsn:encode(Packet, Struct).
```
Координаты вычисляются как элементы массива, начиная с 0.

###Фреймование данных
Обычно, если требуется передать большой объем данных, то для удобства его нарезают на куски, 
для возможности мультиплексирования передачи. В кодировщике реализован механизм автоматического
фреймования. Для этого требуется лишь указать Pid процесса, который будет получать фреймы. Например, так:

```Erlang
   Bin = llsn:encode(Packet, Struct, self()).
```

По-умолчанию, размер фрейма равен 49152 байт (?LLSN_DEFAULT_FRAME_LIMIT). Однако, его можно изменить,
указав дополнительным аргументом при вызове функции кодирования

```Erlang
   Bin = llsn:encode(Packet, Struct, self(), 128000).
```

Если процесс, принимающий фрейм обслуживает параллельно множество процессов кодирования, то для определения
принадлежности фрейма предусмотрен аргумент userdata. Это значение типа list.

```Erlang
receive_frame(Bin) ->
    receive
        {frame, _N, _Size, Frame, UserData } ->
            ?assert(UserData =:= [userdata]),
            receive_frame(<<Bin/binary,Frame/binary>>);

        {done, _N, _Size, Frame, UserData} ->
            ?assert(UserData =:= [userdata]),
            <<Bin/binary,Frame/binary>>

    after 1000 -> ?assert("timeout")
    end.

...

demoframe() ->
...
    ok  = llsn:encode(Packet, Struct, self(), 50, [userdata]),
    Bin  = receive_frame(<<>>),
```

###Декодирование неполных данных
Если закодированные данные разбиты на фреймы, то для декодирования необходимо либо склеить данные самостоятельно

```Erlang
   Bin = <<Frame1/binary, FrameN/binary>>
```

но лучше не думать об этом, а позволить библиотеке обработать это самостоятельно.

```Erlang
    case decode(Bin) of
        {parted, State} ->
            % это не полный пакет, требуется получить следующий фрейм
            ...
        Packet ->
            % пакет декодирован полностью
            ...
    end
    ...
```

После получения следущего фрейма, продолжаем декодирование с того места, где остановились

```Erlang
    Value = decode(continue, State, NextFrame)
```

Этот функционал особенно полезен в случае, если в данные содержат большие файлы. Декодированные данные файлов будут сразу же записываться на жесткий диск. Каталог хранения временных файлов определен в макропеременной ?LLSN_DEFAULT_DIR

Лицензия
--------
Данная библиотека распространяется под лицензией GPL версии 3