-record(request, {created_date,
		  user_name,
		  price}).

-record(transaction, {create_date,
		      tool_name,
		      price,
		      buy_price,
		      sell_price}).

-record(request_price_count, {price, count}).
