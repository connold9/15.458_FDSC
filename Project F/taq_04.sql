use taq04
select 
Q.date,
Q.time_m,
Q.ex,
Q.sym_root,
Q.sym_suffix,
Q.bid,
Q.bidsiz,
Q.ask,
Q.qu_cond,
Q.bidex,
Q.askex,
Q.natbbo_ind,
Q.nasdbbo_ind,
T.tr_scond,
T.size,
T.price,
T.tr_source
from dbo.quote_msec Q
inner join dbo.trade_msec T on (Q.time_m = T.time_m) and (Q.date = t.date) and (Q.sym_root = T.sym_root)
where Q.date = '2015-08-24'
and Q.sym_root IN ('AIG','DIA', 'KO','PG')