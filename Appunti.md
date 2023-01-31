# **Appunti Erlang**

## **Compilazione**

Un programma Erlang può essere compilato in due modi:
- Con il comando `erlc filename.erl`
- All'interno della shell con `c(filename).`

## **Esecuzione**

Dopo essere stato compilato, un programma Erlang può essere eseguito in vari modi, noi li eseguiremo sempre all'interno della shell con `modulename:function_name(args).`

## **Esempio: fattoriale**

```erlang
-module(fact).
-export([fact/1]).

fact(N) -> fact_aux(1, N).

fact_aux(R, 1) -> R;
fact_aux(R, N) -> fact_aux(R*N, N-1).
```

L'attributo `module` permette di definire il nome del modulo (il nome dev'essere un atomo).

L'attributo `export` permette di stabilire quali funzioni definite all'interno del modulo saranno visibili ed invocabili dall'esterno.   
Queste funzioni vanno inserite in una lista `[fun_name1/arity1, ..., fun_nameN/arityN]` (ogni coppia nome/arità deve avere i tipi atomo/intero).   
Nota: le funzioni `fun_a/1` e `fun_a/2` sono due funzioni diverse.

## **Definire funzioni**

```erlang
fun_name(args1) [when guard1] -> body1; %clause 1
fun_name(argsN) [when guardN] -> bodyN. %clause N
```

Le funzioni vengono definite attraverso diverse clausole, queste vengono esaminate dall'alto verso il basso finche non ne viene trovata una che corrisponda con i parametri della chiamata, a questo punto i parametri vengono legati ed il corpo viene eseguito.   
Nota: solitamente si preferisce l'uso dell'underscore_case per dare il nome alle funzioni.

Le sequenze di guardia possono contenere solo valori booleani (anche espressioni valutate a booleani), alcune Built-In Functions (BIFs), le espressioni `andalso` e `orelse`. Le guardie servono a garantire l'assenza di effetti collaterali. 

## **Print**

Per stampare del testo o dei valori sul terminale è necessario utilizzare la funzione `format` del modulo `io`. Questa restituisce un atomo `ok`.

```erlang
io:format("List ~p have length ~p~n", [list, length(list)])
```

## **Tipi**

I tipi presenti in erlang sono:

### **Numeri**

```erlang
10.        ~> 10
16#FF.     ~> 255
$A.        ~> 65
-12.34e-2. ~> 0.1234
```

`base#value` permette di memorizzare un numero espresso in una certa base.   
`$char` viene usato per i caratteri ASCII (sono interi), lo spazio è l'intero 32.   
Nota: i numeri in erlang possono essere arbitrariamente grandi.

### **Atomi**

```erlang
ciao_mondo
'Ciao Mondo'
```

Un atomo è una sequenza di caratteri che inizia con una lettera minuscola e che può contenere qualsiasi carattere (lo spazio interrompe).   
Se viene racchiuso tra virgolette allora può iniziare anche con una lettera maiuscola e contenere spazi.

### **Tuple**

```erlang
{123, "string", atom, {"another", tuple, 1.23}}
```

Le tuple contengono dei valori eterogenei, possono avere qualsiasi dimensione, tipo e complessità.

### **Liste**

```erlang
[].               ~> []
[1 | []].         ~> [1]
[1 | [2]]         ~> [1, 2]
[{1, 2}, ok, []]. ~> [{1, 2}, ok, []]
```

Anche le liste possono contenere valori eterogenei ma la loro dimensione è variabile.   
Con `length(list) -> integer (>= 0)` è possibile ottenere la lunghezza della lista.

## **Operatori**

Gli operatori comunemente utilizzati sono:
- Operatori aritmetici: `+`, `-`, `*`, `/`, `rem` (resto della divisione), `div` (il risultato è un intero, `a div b` è equivalente a `trunc(a/b)`).
- Operatori relazionali: `==`, `/=` (diverso), `<`, `=<`, `>`, `>=`.
- Operatori logici: `and`, `or`, `not`, `xor`.   
Nota: esistono le versioni degli operatori logici che lavorano su singoli bit: `band`, `bor`, `bnot` e `bxor`.
- Operatori liste: `++` (concatenazione di due liste, appende la seconda in coda alla prima).

## **If**

```erlang
if
    condition1 -> body1;
    conditionN -> bodyN;
    true -> body
end.
```

La condizione `true` corrisponde a qualsiasi condizione, il corpo associato verrà sempre eseguito.

## **List Comprehensions**

```erlang
% [Expression || Qualifier1 , ..., QualifierN]
[Element*Element || Element <- List, is_integer(Element)]
```

Le list comprehensions permettono di generare facilmente nuove liste partendo da altre liste, verificando condizioni ed eseguendo operazioni.   
I qualificatori possono essere generatori (Pattern <- List) o filtri (predicati o espressioni booleane).

## **Modulo Lists**

Il modulo Lists introduce diverse funzioni utili per lavorare con le liste:

```erlang
lists:concat(elements) -> string
% Concatena la rappresentazione testuale degli elementi ricevuti (possono essere atomi, interi, float o stringhe).
lists:enumerate(list) -> list
% Trasforma una lista [a,b,c] in una lista [{1,a}, {2,b}, {3,c}] enumerando i suoi elementi.
lists:filter(predicate, list) -> list
% Restituisce una lista contenente solo gli elementi della lista ricevuta che soddifano un certo predicato.
lists:foldl(function, acc_init, list) -> acc_final
lists:foldr(function, acc_init, list) -> acc_final
% Viene eseguita la funzione specificata su tutti gli elementi della lista, questa funzione modificherà l'accumulatore, terminata l'esecuzione questo conterrà il risultato e verrà restituito.
lists:foreach(function, list) -> ok
% La funzione specificata viene eseguita su tutti gli elementi della lista.
lists:keyfind(key, n, list) -> tuple | false
% Cerca nella lista di tuple ricevuta una tupla che abbia la chiave specificata nella posizione specificata.
% lists:keyfind(world, 2, [{"hello", hello, 1}, {"world", world, 2}]) -> {"world", world, 2}
lists:reverse(list) -> list
% Restituisce la lista invertita.
lists:seq(from, to) -> int_list
lists:seq(from, to, increment) -> int_list
% Restituisce una lista di interi che va dal minimo al massimo specificato (estremi inclusi).
lists:sort(list) -> list
lists:sort(function, list) -> list
% Restituisce la lista ordinata in base alla funzione di ordinamento utilizzata.
lists:sublist(list, length)
lists:sublist(list, start, length)
% Restituisce una sottolista.
lists:unzip(list) -> {list1, list2}
% Separa una lista di tuple in due liste di elementi singoli.
% [{1,a}, {2,b}, {3,c}] -> {[1,2,3], [a,b,c]}
lists:zip(list1, list2) -> list
% Unisce due liste di egual dimensione in una sola lista di tuple.
% {[1,2,3], [a,b,c]} -> [{1,a}, {2,b}, {3,c}]
```

## **Attori**

Per generare un nuovo attore possiamo usare la BIF `spawn`, questa ci restituirà il pid del nuovo attore.

```erlang
spawn/1 ~> spawn(function) -> pid
% spawn(fun() -> loop(args) end) permette di avviare un altro attore che esegue la funzione 'loop' senza averla esportata.
spawn/2 ~> spawn(node, function) -> pid
spawn/3 ~> spawn(module, function_name_as_atom, args_list) -> pid
spawn/4 ~> spawn(node, module, fucntion_name_as_atom, args_list) -> pid
```

Esistono anche le versioni `spawn_link` e `spawn_monitor`.

## **Messaggi**

Per mandare un messaggio ad un altro attore si usa l'operatore `!`.

```erlang
actor ! expression
```

L'invio di un messaggio non fallisce mai (anche se non verrà mai recapitato) e non blocca il mittente.   
Ogni attore ha una mailbox in cui conserva i messaggi ricevuti, sono conservati in ordine di ricezione ma non c'è nulla che imponga di accedervi nello stesso ordine.

```erlang
receive
    Msg = Pattern1 [when guard1] -> body1; % body1 può usare Msg per intero
    PatternN [when guardN] -> bodyN % bodyN può accedere ai singoli componenti del messaggio
after 5000 -> io:format("timeout!~n")
end
```

La clausola `after` permette d'interrompere la ricezione dopo un certo tempo, in modo che non si verifichi un attesa infinita (un attore che esegue la `receive` rimane bloccato finche non trova un messaggio che corrisponda ad un pattern specificato).

In genere è buona norma mettere come ultima clausola prima di `after` un pattern che corrisponda sempre, in questo modo i messaggi non desiderati possano essere scartati.

```erlang
Other -> io:format("Unknown ~p~n", [Other])
```

## **Registrare attori**

Erlang mette a disposizione diverse funzioni per rendere noti e disponibili a più attori i pid di altri attori.

```erlang
register(name_as_atom, pid)
unregister(name_as_atom)
whereis(name_as_atom) -> pid | undefined
```

È possibile mandare un messaggio ad un attore che è stato registrato semplicemente con `registered_name ! message` se l'altro attore è locale, o con `{registered_name, node} ! message` se è remoto.

## **Gestione errori**

Se vogliamo fare in modo che un attore sappia quando e perchè è terminato un altro attore, dobbiamo ricorrere alla funzione `link(pid)` (o a `spawn_link`).

I link sono dei collegamenti simmetrici (se uno termina l'altro viene avvisato), se invece vogliamo creare un collegamento asimmetrico (monodirezionale) serve un monitor.

```erlang
erlang:monitor(process, pid)
```
Se l'attore monitorato termina, quello che ha creato il monitor riceverà un messaggio `{'DOWN', Ref, process, pid, reason}`.

Se due attori sono linkati, quando uno dei due termina (naturalmente o a causa di un errore) l'altro riceverà un messaggio `{'EXIT', pid, reason}`.   
Solitamente questi messaggi vengono ignorati, per poterli elaborare tramite una `receive` è necessario rendere l'attore un attore di sistema con:

```erlang
process_flag(trap_exit, true)
```

I segnali di uscita sono generati con `exit(reason) -> {'EXIT', pid, reason}`, quando un processo termina normalmente viene implicitamente chiamata `exit(normal) -> {'EXIT', pid, normal}`.   
È possibile mandare un segnale di uscita ad un altro attore senza terminare effettivamente (finta morte) con `exit(pid, reason)`.

Tutti gli attori che non sono di sistema terminano quando ricevono un segnale di uscita, quelli di sistema invece ricevono il segnale di uscita nella loro mailbox e possono intraprendere diverse azioni (es: ricreare l'attore che è terminato).

Se un attore riceve un messaggio `{'EXIT', pid, kill}` terminerà sempre, anche se è un attore di sistema.



## **Distribuzione**

Per far funzionare una shell in modalità distributa basta avviarla usando `erl -sname name`, in questo modo prenderà il nome `name@hostname`.

Funzioni utili quando si lavora con nodi distributi:

```erlang
group_leader(whereis(user), self())
% Permette di stampare il testo sul terminale locale, anche se l'attore è stato creato da remoto
{ok, Hostname} = inet:gethostname()
% Permette di ottenere il nome della macchina host in forma di stringa
list_to_atom("client@"++Hostname)
% Permette di creare un atomo che rappresenta il nome completo di un nodo
% spawn_link(list_to_atom("server@"++Hostname), server, init, [self()])
nodes()
% Restituisce l'elenco dei nodi connessi con quello che la esegue, può essere stampata direttamente (il testo è gia formattato)
```

## **Creazione anelli**

A volte potremmo voler creare degli anelli di attori che comunicano tra di loro (il precedente con il successivo), il modo migliore per farlo è utilizzare una funzione ricorsiva.

Il gestore dell'anello fa la `spawn` del primo nodo, questo crea il secondo e così via.   
L'ultimo nodo manda un messaggio al gestore per comunicargli l'avvenuta creazione (poi il gestore può iniziare a mandare messaggi all'anello).