package com.edawg878.bukkit.plot

import java.time.Instant
import java.util.UUID

import com.edawg878.common.PlotRepository
import org.bukkit.ChatColor._
import org.bukkit.{Server, World}
import org.bukkit.conversations._
import org.bukkit.entity.Player
import org.bukkit.plugin.Plugin
import com.edawg878.common.Color.Formatter
import scala.collection.mutable

/**
 * @author EDawg878 <EDawg878@gmail.com>
 */
object PlotClearConversation {

  class PlotClearConversation(resolver: PlotWorldResolver, plugin: Plugin, plotDb: PlotRepository, server: Server) {

    val pending = mutable.Map[UUID, (UUID, PlotId)]()

    val factory = new ConversationFactory(plugin)
      .withModality(true)
      .withPrefix(new ConversationPrefix {
        override def getPrefix(ctx: ConversationContext): String = BLUE + "[Plot Genie] "
      })
      .withFirstPrompt(new AskPrompt)
      .withTimeout(30)
      .addConversationAbandonedListener(new ConversationAbandonedListener with AskingPrompt {
        override def conversationAbandoned(ev: ConversationAbandonedEvent): Unit = {
          val ctx = ev.getContext
          val p = ctx.getForWhom.asInstanceOf[Player]
          pending.remove(p.getUniqueId) map {
            case (wid, id) =>
              if (getAnswer(ctx).getOrElse(false)) {
                p.sendMessage(info"The plot genie is now clearing your plot")
                Option(server.getWorld(wid)) map { bw =>
                  resolver(bw) map { w =>
                    w.clear(bw, id)
                    w.getPlot(id) map { plot =>
                      val updated = plot.copy(lastCleared = Some(Instant.now))
                      w.update(updated)
                      plotDb.save(updated)
                    }
                  }
                }
              }
          }
          if (!ev.gracefulExit)
            p.sendMessage(err"You have lost the plot genie's attention")
        }
      }).withInitialSessionData {
        val data = new java.util.HashMap[AnyRef, AnyRef]()
        data.put("answer", None)
        data
      }

    def begin(p: Player, id: PlotId): Unit = {
      pending.put(p.getUniqueId, (p.getWorld.getUID, id))
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
