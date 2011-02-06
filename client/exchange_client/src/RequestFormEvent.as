package
{
	import flash.events.Event;
	
	public class RequestFormEvent extends Event
	{
		public static const REQUEST_BUY:String = "RequestBuy";
		public static const REQUEST_SELL:String = "RequestSell";
		
		private var _price:Number;
		
		public function RequestFormEvent(type:String, price:Number)
		{
			super(type, false, false);
			_price = price;
		}
		
		public function get price():Number
		{
			return _price;
		}
	}
}