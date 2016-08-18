asn1encode - сериализатор для ACP пакетов.

##Настройка
Для использования необходимо добавить asn1encode как зависимость, прописав его в deps файла rebar.config ссылку на данных репозиторий.
Далее для настройки проекта необходимо в папке с asn1encode выполнить следующий bash скрипт:

```bash
rebar get-deps && rebar compile && ./relx release
```
или запустить [compile.sh](https://github.com/borisbochkaryov/asn1encode/complite.sh) из папки с asn1encode.

Так же для корректной работы фреймворка chronica с asn1encode необходимо дописать следующие правила:
```erlang
{rules,[
            {asn1encode,            "asn1encode",   info,       [encode_decode_pack], on},
            {asn1error,             "asn1error",    error,      [error_pack], on}
        ]}
```

И именованные потоки:
```erlang
{flows,[
            {encode_decode_pack, [{file, "asn1encode.log"}]},
            {error_pack, [{file, "asn1error.log"}]}
        ]},
```

##Использование
Сериализация:
```erlang
Binary = asn1:encode(Pack).
```

Десериализация:
```erlang
Result = asn1:decode(Binary).
```