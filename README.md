acp_coder - сериализатор для ACP пакетов.

##Настройка
Для использования необходимо добавить acp_coder как зависимость, прописав его в deps файла rebar.config ссылку на данных репозиторий.
Далее для настройки проекта необходимо в папке с asn1encode выполнить следующий bash скрипт:

```bash
rebar get-deps && rebar compile && relx release
```

Так же для корректной работы фреймворка chronica с asn1encode необходимо дописать следующие правила:
```erlang
{rules,[
       {acpCoder,            "acpCoder",         info,       [encode_decode_pack], on},
       {acpCoderError,       "acpCoderError",    error,      [error_pack],         on}
]}
```

И именованные потоки:
```erlang
{flows,[
       {encode_decode_pack, [{file, "acp_coder.log"}]},
       {error_pack,         [{file, "acp_coder_error.log"}]}
]},
```

##Использование
Сериализация:
```erlang
Binary = acp_coder:encode(Pack).
```

Десериализация:
```erlang
Result = acp_coder:decode(Binary).
```
