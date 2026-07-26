# Template canonico di un capitolo

Riferimento editoriale per *Probabilità per la psicologia*.
Verificato automaticamente da `R/check_template.R`.

## Struttura

```
# Titolo del capitolo {#sec-slug}

::: {.callout-note title="In sintesi"}
**Da sapere**  ...
**Da saper fare**  ...
:::

## Introduzione                       (non numerata)
   Aggancio al capitolo precedente, domanda motivante,
   rilevanza per la psicologia.

::: {.callout-tip title="Prerequisiti"}
   Capitoli da aver letto + appendici matematiche richiamate.
   Assente solo nel capitolo 1.
:::

## Panoramica del capitolo            (non numerata)
   L'elenco dei passaggi che il capitolo compie.

::: {.callout-note title="Preparazione del notebook"}
   Blocco di setup (source di _common.R, pacchetti, tema).
:::

## 1 ... N                            SEZIONI NUMERATE
   Il contenuto vero e proprio.

## Riflessioni conclusive             (non numerata)
   Chiusura narrativa: che cosa si è ottenuto, che cosa
   resta aperto, ponte verso il capitolo successivo.

## Esercizi                           (non numerata)
   ::: {.callout-tip title="Soluzioni" collapse="true"}
   :::

::: {.callout-note title="Punti chiave" collapse="true"}
   Promemoria consultabile: definizioni e formule.
:::

::: {.callout-note title="Ambiente di sviluppo" collapse="true"}
   sessionInfo()
:::

## Bibliografia                       (non numerata, solo se citata)
```

## Regole trasversali

1. Ogni capitolo ha un'etichetta `{#sec-slug}` sul titolo di primo
   livello, stabile nel tempo: è ciò a cui puntano i rimandi.
2. Le sezioni di apertura e chiusura non sono numerate; solo il
   corpo lo è. `number-depth: 2` in `_quarto.yml` impedisce che la
   numerazione tracimi nelle sottosezioni.
3. Il callout di preparazione è `callout-note`, non
   `callout-warning`: non avverte di un pericolo, prepara l'ambiente.
4. I callout lunghi e consultabili (Soluzioni, Punti chiave,
   Ambiente di sviluppo) hanno `collapse="true"`.
5. Ogni rimando usa `@sec-...`; nessun capitolo cita un'etichetta
   che non esista nel progetto.
6. Gli esempi psicologici stanno in `callout-note`; gli errori da
   evitare in `callout-warning`; i messaggi da ricordare in
   `callout-important`.

## Deroghe ammesse

- Il capitolo 1 non ha Prerequisiti.
- Le appendici non hanno Panoramica, Esercizi né Riflessioni
  conclusive: sono materiale di consultazione, non di percorso.
- Bibliografia compare solo dove ci sono citazioni.
