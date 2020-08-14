use taq

select * from dbo.trade T
inner join dbo.dim_time DT on (T.time  = DT.t)
where T.date between '2010-05-05' and '2010-05-06'
and T.symbol in ('AIG','DIA', 'KO','PG')