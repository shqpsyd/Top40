import scala.io.Source
import scala.collection.mutable.Map 
import util.control.Breaks._
//can change topNWord,topNSinger40,topNSinger1,paticularWord,travial to whatever is necessary
object Top40 {
	val topNWord = 10
	val topNSinger40 = 10
	val topNSinger1 = 10
	val paticularWord = "RAIN"
	val travial = List("TO","OF","IN","THE","AND","BABY","BE","IS","FOR","UP","ITS","A","ON")
	def main(args:Array[String]): Unit = {
		var songContainWord = 0
		var songIsTop = 0
		var list = List[Tuple3[String,String,Int]]()
		val freqWords = Map[String,Int]()
		val fourtySinger = Map[String,Int]()
		val firstSinger = Map[String,Int]()
		for(line<-Source.fromFile("top40.sql").getLines()) {
			val newline1 = line.split("values")(1) replaceAll("[();*-/.,\\\\]","")
			val newline2 = newline1.trim.replaceAll(" +", " ")
			val regex="""{0,10}[0-9]+ '((?:\s?\w+\s?)+)' '((?:\s?\w+\s?)+)' ([0-9]+)""".r
			val regex(artist,title,flag) = newline2
			list = (artist.trim,title.trim,flag.toInt)::list		
		}

		for(tup<-list){
			for(word<-tup._2.split(" ")){
				if(freqWords.contains(word))
					freqWords(word)+=1
				else
					freqWords+=(word->1)
			}
		}		
		val mostFreq = collection.mutable.LinkedHashMap(freqWords.toSeq.sortWith(_._2 > _._2):_*)
		println("The top "+topNWord +" frequent non trivial words:")
		var i = 0
		breakable{
			for(word<-mostFreq){
				if (i < topNWord && travial.contains(word._1)==false){
					println(word._1+" "+word._2)
					i+=1
				}
				else if(i == topNWord)
					break
			}	
		}
		println
		for(tup<-list){
			if(fourtySinger.contains(tup._1)){
				fourtySinger(tup._1)+=1
			}
			else{
				fourtySinger+=(tup._1->1)
			}
			if(tup._3 == 1){
				if(firstSinger.contains(tup._1)){
					firstSinger(tup._1)+=1
				}
				else{
					firstSinger+=(tup._1->1)
				}				
			}
		}
		val most40 = collection.mutable.LinkedHashMap(fourtySinger.toSeq.sortWith(_._2 > _._2):_*)
		i = 0
		println("artists with the most top 40 songs:" )
		breakable{
			for(singer<-most40){
				if (i < topNSinger40 ){
					println(singer._1+" "+singer._2)
					i+=1
				}
				else break
			}	
		}
		println
		val most1 = collection.mutable.LinkedHashMap(firstSinger.toSeq.sortWith(_._2 > _._2):_*)
		i = 0
		println("artists with the number one songs:" )
		breakable{
			for(singer<-most1){
				if (i < topNSinger1 ){
					println(singer._1+" "+singer._2)
					i+=1
				}
				else break
			}	
		}
		println
		for(tup<-list){
			var flag = 0
			for(word<-tup._2.split(" ")){
								
				if(word == paticularWord && flag == 0){
					
					songContainWord +=1	
					flag = 1
					if(tup._3 == 1){
						songIsTop+=1						
					}				
				}			
			}
		}
		if(songContainWord > 0)
			println("P(a song being number one| the title contains "+paticularWord+") is " +songIsTop/songContainWord.toFloat)
		else
			println("No top song contains"+paticularWord)
		println
		songIsTop = 0
		songContainWord = 0
		for(tup<-list){
			var flag = 0
			if(tup._3 == 1){
				songIsTop+=1
				for(word<-tup._2.split(" ")){
					if(word == paticularWord && flag == 0){
						songContainWord += 1
						flag = 1
					}
				}
			}			
		}
		if(songIsTop>0)
			println("P(the title contains "+paticularWord+ "| a song being number one) is " +songContainWord/songIsTop.toFloat)
		else
			println("No top song contains"+paticularWord)
	}
}


