class Main{
	public static void main(String Args[]){
	arraylists();
	}

	private static void arraylists(){
		UserGroup users = new UserGroup();
		users.addSampleData();
		users.printUsernames();
	}
}