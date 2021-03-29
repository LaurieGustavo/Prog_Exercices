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
	val players	: List[Player]	= glMap.values.toList.flatten.distinct
	
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
	
	// Get new GameLibrary of one player only
	def gamesOf(p: String): GameLibrary = {
		new GameLibrary(glMap.filter(_._2.exists( _.name == p)).map{
			case(k,v) => (k, v.filter(_.name == p))
		})
	}

	// Get new GameLibrary whit only games every player in p have in common
	def commonGamesOf(p : List[String]): GameLibrary = {

		new GameLibrary(glMap.filter{
			case(k,v) => p.forall(v.map(_.name).contains(_))
		}.map{
			case (k,v) => (k, v.filter( x => p.contains(x.name)))
		})
	}

	// return the Union of games possessed by at least one player of list p
	def unionGamesOf(p : List[String]): GameLibrary = {

		new GameLibrary(glMap.filter{
			case(k,v) => p.exists(v.map(_.name).contains(_))
		}.map{
			case(k,v) => (k, v.filter(x => p.contains(x.name)))
		})
	}

	// return a new GameLibrary with games playable by x players
	def forGroupSize(i: Int): GameLibrary = {
		new GameLibrary(glMap.filter{
			case(k,v) => k.minPlayers <= i && i <= k.maxPlayers 
		})
	}

	// get a new GameLibrary with games of players who possess game g
	def playersOf(g: String): GameLibrary = {
		this.unionGamesOf(glMap(games.filter(_.name == g)(0)).map(_.name))
	}

	// return the size of the GameLibrary, which is the number of games
	def size() = games.size


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

	BTG.commonGamesOf(List("Les Chléos", "Gustavo")).displayGames()
	BTG.playersOf("Hanabi").displayPlayers()

}
