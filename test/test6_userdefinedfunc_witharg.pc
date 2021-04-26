int testcall(int b){
	b = b + 1;
	printi(b);
	if(b > 5){
		return 0;
	}else{
		testcall(b);
	}
	return -1;
}

void main(){
	int i=0;
	
	testcall(0);
}
