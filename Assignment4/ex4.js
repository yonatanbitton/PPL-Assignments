/////////////////////////////////// Q1 ////////////////////////////////
function all(promises) { //gets an array of promises. return a new promise. The content :
						 // if all promises are fulfilled, returns array of their result.
						 // If any of the promises fails, returned promise should fail. 
   
   return new Promise( (resolve, reject) => {
   		if (promises.length===0) resolve([]);
   		let ans=[];
   		let ans_length = promises.length;
    	promises.forEach(
    		(promise,i)=>{
    			promise.then((content)=>{
    				//ans=ans.concat([content]);
            //ans.push(content);
    				ans[i]=content;
    				ans_length--;
	    			if (ans_length===0) { resolve (ans);}
    		}).catch((err)=>reject(err));
  		});
   })
}


 
// Returns a generator that returns i, each step it's being called.

function* take(n, generator) {
    for (let x of generator) {
        if (n <= 0) return;
        n--;
        yield x;
    }
}

function* naturalNumbers(){
  let n=0;
  while(1){
    yield n;
    n++;
  }
} 

function* from2() {
 for (let n=2;; n++) {
 yield n;
 }
}


function* filterGen(gen, pred){
  for (x of gen){
    if (pred(x)) yield x;
    // The else is "already written" - goes for next item at gen defultively.
  }
}


function* sieve (generator){
  while(true){
    let res = generator.next();
    let x = res.value;
    if (res.done) {
      return x;
    }
    generator=filterGen(generator,z=>z%x!=0);
    yield x;
  }
}


