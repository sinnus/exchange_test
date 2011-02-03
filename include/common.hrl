-record(request, {id,
		  created_date,
		  user_name,
		  type,
		  price}).

-record(transaction, {created_date,
		      tool_name,
		      price,
		      buy_user,
		      buy_price,
		      sell_user,
		      sell_price}).

-record(request_price_count, {price, count}).
