class FizzBuzz{
	public static void main(String Args[]){

	for(Integer i = new Integer(1); i < 61; i++){//loop from 1 to 61
	if(i % 3 == 0){
		System.out.print("Fizz");
	}/*if i is divisible by 3 then Fizz is printed*/
	if(i % 5 == 0){
		System.out.print("Buzz");
	}/*if i is divisible by 5 then Buzz is printed*/
	if(i % 3 != 0 && i % 5 != 0){
	System.out.print(i);
	}/*if i is not divisible by 3 nor 5 then i is printed*/
	System.out.println();//skips to the next line
	}
}
}


















