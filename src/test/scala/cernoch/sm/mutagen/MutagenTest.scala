package cernoch.sm.mutagen

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MutagenTest extends Specification {

  "Movie dataset" should {

    "read all records in the easy domain" in {
      (new Mutagen(true)).dump.size must_== 20594
    }

    "read all records in the hard domain" in {
      (new Mutagen(false)).dump.size must_== 4104
    }
  }
}