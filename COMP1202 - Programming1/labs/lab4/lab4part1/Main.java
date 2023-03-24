import java.util.*;
class Main{
	public static void main(String Args[]){
	loop();
	}
	private static void loop(){
		Toolbox myToolbox = new Toolbox();
		int a=myToolbox.readIntegerFromCmd();
		for (int i=1; i<21;i++){
			int b = a * i;
			System.out.println(b);
		}
		System.out.println("");
		int c =0;
		int d = 1;
		int e = 0;
		while (c < 500) {
			c = c + d;
			d++;
			e++;
		}
		System.out.println(e+" Iterations were required to get to 500");
	}
}