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
%% Value objects (For JSON)
-record(login_vo, {login, password}).
-record(request_price_count_vo, {price, count}).
-record(event_vo, {type, data}).
-record(request_data_vo, {price, tool}).
-record(top_10_requests_vo, {buy_requests, sell_requests}).
-record(transaction_vo, {tool_name, price}).
