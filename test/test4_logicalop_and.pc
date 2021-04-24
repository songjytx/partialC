void main(){
	bool a = true;
	bool b = false;
	if(a && b){
		prints('wrong');
	}else{
		prints('correct');
	}
	if(a && a){
		prints('correct');
	}else{
		prints('wrong');
	}
}