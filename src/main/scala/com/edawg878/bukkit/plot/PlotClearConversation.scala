package com.edawg878.bukkit.plot

import java.time.Instant
import java.util.UUID

import com.edawg878.common.PlotRepository
import org.bukkit.ChatColor._
import org.bukkit.World
import org.bukkit.conversations._
import org.bukkit.entity.Player
import org.bukkit.plugin.Plugin
import com.edawg878.common.Color.Formatter
import scala.collection.mutable

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object PlotClearConversation {

  class PlotClearConversation(pms: World => Option[PlotManager], plugin: Plugin, plotDb: PlotRepository) {

    val prefix = new ConversationPrefix {
      override def getPrefix(ctx: ConversationContext): String = BLUE + "[Plot Genie]"
    }

    val abandonedListener = new ConversationAbandonedListener with AskingPrompt {
      override def conversationAbandoned(ev: ConversationAbandonedEvent): Unit = {
        val ctx = ev.getContext
        val p = ctx.getForWhom.asInstanceOf[Player]
        pending.remove(p.getUniqueId) map { id =>
          if (getAnswer(ctx).getOrElse(false)) {
            p.sendMessage(info"The plot genie is now clearing your plot")
            pms(p.getWorld) map { pm =>
              pm.clear(id)
              pm.getPlot(id) map { plot =>
                val updated = plot.copy(lastCleared = Some(Instant.now))
                pm.update(updated)
                plotDb.save(pm.w, updated)
              }
            }
          }
        }
        if (!ev.gracefulExit)
          p.sendMessage(err"You have lost the plot genie's attention")
      }
    }

    val initialData = {
      val map = new java.util.HashMap[AnyRef, AnyRef]()
      map.put("answer", None)
      map
    }

    val factory = new ConversationFactory(plugin)
      .withModality(true)
      .withPrefix(prefix)
      .withFirstPrompt(new AskPrompt)
      .withTimeout(30)
      .addConversationAbandonedListener(abandonedListener)
      .withInitialSessionData(initialData)

    val pending = mutable.Map[UUID, PlotId]()

    def begin(p: Player, id: PlotId): Unit = {
      pending.put(p.getUniqueId, id)
      factory.buildConversation(p).begin()
    }

  }

  trait AskingPrompt {

    def getAnswer(ctx: ConversationContext): Option[Boolean] = ctx.getSessionData("answer").asInstanceOf[Option[Boolean]]

    def setAnswer(ctx: ConversationContext, a: Boolean): Unit = ctx.setSessionData("answer", Some(a))

  }


  class AskPrompt extends BooleanPrompt with AskingPrompt {

    override def acceptValidatedInput(ctx: ConversationContext, yes: Boolean): Prompt = {
      if (yes) {
        if (getAnswer(ctx).isDefined) new YesPrompt
        else {
          setAnswer(ctx, true)
          new AskAgainPrompt
        }
      } else {
        setAnswer(ctx, false)
        new NoPrompt
      }
    }

    override def getPromptText(context: ConversationContext): String = info"Are you sure you want me to clear the plot?"
  }

  class AskAgainPrompt extends MessagePrompt {
    override def getNextPrompt(context: ConversationContext): Prompt = new AskPrompt

    override def getPromptText(context: ConversationContext): String = info"Okay, I will ask one more time just to make sure."
  }

  class YesPrompt extends MessagePrompt {
    override def getNextPrompt(context: ConversationContext): Prompt = Prompt.END_OF_CONVERSATION

    override def getPromptText(context: ConversationContext): String = info"Okay, I will clear the plot now."
  }

  class NoPrompt extends MessagePrompt {
    override def getNextPrompt(context: ConversationContext): Prompt = Prompt.END_OF_CONVERSATION

    override def getPromptText(context: ConversationContext): String = info"Okay, I won't clear your plot."
  }

}
