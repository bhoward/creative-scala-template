package music

case class Song(title: String, parts: Part*)

trait Part

case class CompoundPart(parts: Part*) extends Part

case class InstrumentPart(instrument: Instrument, events: Event*) extends Part

trait Instrument

trait Event:
  def duration: Duration

