// Case Classes

// Case Class Player
case class Player(name : String)
// Case Class Game extends Ordered to get list of games sorted by name
case class Game(name : String, minPlayers: Int = 0, maxPlayers: Int = Int.MaxValue) extends Ordered[Game]{
	def compare (that: Game) = this.name compare that.name
}

/*----------------------------------------------------------------*/

class GameLibrary( val glMap : Map[Game, List[Player]]){

	// Values
	val games 	: List[Game] 	= glMap.keys.toList
	val players	: List[Player]	= glMap.values.filter(_.size < 2).toList.flatten.distinct
	
	/*----------------------------------------------------------------*/

	// Functions

	// Display the list of games
	def displayGames() : Unit = {

		def displayGame(g : Game): Unit = {
			if ( g.minPlayers == g.maxPlayers) {
				println("game : %s, \nplayers number : %d".format(g.name, g.minPlayers))
			} else if (g.minPlayers == 0 && g.maxPlayers == Int.MaxValue) {
				println("game : %s, \nplayers number : unkown".format(g.name))
			} else {
				println("game : %s, \nplayers number : %d-%d".format(g.name, g.minPlayers,g.maxPlayers))
			}
		}
		games.sorted.foreach(displayGame(_))
	}

	// display the list of players
	def displayPlayers() : Unit = {
		players.foreach(p => println(p.name))
	}


	/*
	

	// return a list of games of player p
	def gamesOf(p : String) : List[String] = {
		libraryMap.filter(_._2.contains(p)).keys.toList
	}

	// return number of games of player Player
	def gamesNbOf(p: String): Int = {
		gamesOf(p).size
	}

	// return the common games of a list of players p
	// that is, all the games that are possessed by every player in the list p
	def commonGamesOf(p : List[String]): List[String] = {
		libraryMap.filter(g => p.forall(g._2.contains(_))).keys.toList
	}

	// return list of games possessed by at least one player of list players
	def unionGamesOf(p: List[String]): List[String] = {
		libraryMap.filter(g => p.exists(g._2.contains(_))).keys.toList
	}

	// return the list of games that you can play with i players
	def gamesForGroupSize(i: Int): List[String] = {
		games.filter(g => g.minPlayers <= i &&  i <= g.maxPlayers).map(_.name)
	}
	*/

}

// Object Companion to create a GameLibrary form a csv file
object GameLibrary {

	def FromFile( googleSheet: String): GameLibrary = {

		// function to transform a line of csv into a Game
		def gameFromLine(s: List[String]): Game = s(1) match {
			case "" =>	new Game(s(0))
			case x  =>  val playersNB = s(1).split("-").toList.map(_.toInt)
						new Game(s(0), playersNB.min, playersNB.max)
		}

		// Create a tuple Game -> list of possessors
		def gameMapping(s: List[String], p: List[String]): (Game,List[Player]) ={
			val gameName 	= s(0)
			(gameFromLine(s), (p zip s.drop(2)).filter{ case(k,v) => v != ""}.map{case(k,v)=> new Player(k)})
		}	

		val iter 			= io.Source.fromFile(googleSheet).getLines()
		val playersName 	= iter.next().split(",").toList.tail.tail
		val gamelibraryStr	= iter.toList.map(_.split(",",-1).map(_.trim).toList)
		val gamelibraryMap 	= gamelibraryStr.map(gameMapping(_, playersName)).toMap

		new GameLibrary(gamelibraryMap)
	}
}

object TestLibrary extends App {

	// Bubble Team GameLibrary
	val BTG : GameLibrary = GameLibrary.FromFile("jeux.csv")

	BTG.displayGames()
	val lama = BTG.glMap.filter(_._2.exists( _.name == "Gustavo")).map{
		case(k,v) => (k, v.filter(_.name == "Gustavo"))
	}

	println(lama)

}
