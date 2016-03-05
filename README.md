# Memos

HaskellとPureScriptで作った簡単なメモアプリ

## Frontend
PureScript製。purescript-halogen,purescript-argonaut,purescript-affjaxなどを使用した。

~~~
frontend $ npm install
frontend $ bower update
frontend $ npm run build
~~~

でビルドできるはず...

~~~
frontend $ ./server.hs
~~~

でサーバーが立つ。下記のBackendサーバーを起動してから`http://localhost:8081/`をブラウザで開く。

## Backend
Haskell製。servant-server, servant-client, persistent, persistent-sqliteなどを使用した。

~~~
backend $ stack build
backend $ stack exec memos-server
~~~

でサーバーが立つ。

CLIクライアントもある。

~~~
backend $ stack exec -- memos-cli [command]
~~~

で使用できる。
