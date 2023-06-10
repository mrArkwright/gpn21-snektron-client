import java.io.DataOutputStream

val chatMessages: List[String] = List(
  "You will all die",
  "I'm comming after all of you",
  "I will kill you all",
  "All of you are doomed",
  "The end is near",
  "Your fates are sealed.",
  "There is no escape from the darkness.",
  "The shadows grow closer.",
  "Oblivion awaits.",
  "The final hour approaches.",
  "Your destinies are written in blood.",
  "The sands of time are running out.",
  "Embrace the inevitable downfall.",
  "There is no hope, only despair.",
  "Your last breath is nigh.",
  "The shroud of fate tightens its grip.",
  "The cataclysm has begun.",
  "Winds of destruction blow in your direction.",
  "Darkness shall consume all.",
  "Beware, for the reaper draws near.",
  "Your efforts are but whispers in the storm.",
  "The bell tolls for the end of days.",
  "Chaos reigns, your doom is at hand.",
  "The abyss gazes into your souls.",
  "The night falls, and with it, your hope.",
  "The stars have foretold your demise.",
  "Ashes to ashes, all shall fade.",
  "Cower before the storm of despair.",
  "This is the hour of reckoning.",
  "Eternal darkness shall be your final sanctuary.",
  "There is no light at the end, only the void.",
  "The echoes of your screams will fade into silence.",
  "Fear the approaching shadow, it heralds your end.",
  "What was once yours will be reclaimed by the abyss.",
  "The chains of fate are unbreakable; surrender to the void."
)

def sendRandomChatMessage(outputStream: DataOutputStream): Unit = {
  val randomMessage = chatMessages(scala.util.Random.nextInt(chatMessages.length))
  sendMessage(ClientMessage.Chat(randomMessage), outputStream)
}

