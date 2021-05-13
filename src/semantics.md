# 意味論

$$
\begin{aligned}
Program =& Statement* \\
Statement(t) =& ~Metadata \\
             |& ~Declaration \\
             |& ~Definition \\
             |& ~Expression \\
    
\end{aligned}\\
\begin{aligned}
t =&\\
    & true\\
    & false\\
    & i           &(\text{integer literal})\\
    & f           &(\text{floating number literal})\\
    & s           &(\text{string literal})\\
    & fn          &(\text{functin leteral})\\
    & x           &(\text{variable, ident})\\
    & t(t)        &(\text{applying function})\\
    & \text{if}~ t ~\text{then}~ t ~\text{else}~ t
                  &(\text{if expression})\\
    & loop: t     &(\text{loop expression})\\
    & block~t    &(\text{block expression})\\
    & ![x: t]     &(\text{metadata})\\
    & var x = t   &(\text{vardecl})\\
    & let x = t   &(\text{letdecl})\\
    & const x = t &(\text{constdecl})\\
    & alias x = t &(\text{aliasdecl})\\
    & func x[T: t, ...](x: T, ...) -> T: t
                  &(\text{func definition})\\
    & t; t        &(\text{seq})\\
    & T           &(\text{type as expr})\\

T =&\\
    &int\\
    &float\\
    &string\\
    &bool\\
    &Unit\\
    &(T, T, ...) &(\text{tuple})\\
    &(T, ...) -> T
                &(\text{function type})\\
    &(x: T, ...) &(\text{record})\\
    &ref ~ T    &(\text{reference})\\
    &typeof(t)\\
    &t           &(\text{expr as type})\\
    &l           &(\text{literal})\\
\Gamma =&\\
    & \phi\\
    & \Gamma,x: T
\end{aligned}

$$
## 型付け規則
$$
\begin{aligned}
&true : bool\\
&false : bool\\
&i : int & \text{T-Lit}\\\\
&\frac{t_1 : bool ~~ t_2 : T ~~ t_3 : T}{\text{if} ~ t_1 ~\text{then}~ t_2 ~\text{else} t_3 : T}& \text{T-if} \\\\
&\frac{\Gamma \vdash t_1 : \text{Unit} ~~ \Gamma \vdash t_2 : T}{t_1; t_2 : T}& \text{T-Seq} \\\\
&\frac{\Gamma \vdash t_1 : T_1 ~~ \Gamma \vdash t_2 : T_2}{break~t_1; t_2 : T_1}& \text{T-Seq} \\\\
&\frac{\Gamma\vdash t_1: T_1\to T_2, t_2: T_1}{t_1(t_2): T_2}&\text{T-App}\\\\
&\frac{\Gamma\vdash t: T}{\text{block}~t: T}&\text{T-Block}\\\\
&\vdash ![x: t]: Unit&\text{T-Unit}\\\\
&\text{var}~ x = t, t: T\vdash x: T&\text{T-VarDecl}\\\\
&\frac{\Gamma, x_i: T_i, ...\vdash t: T}{\text{func}~x(x_i: T_i)\to T:~t, x: (x_i: T_i, ...) \to T}&\text{T-FuncDef}\\\\

\end{aligned}
$$

let a = loop:
    update hoge
    if hoge:
        break 1
assert a == 1