package org.goldenport.context

import org.goldenport.exception.GoldenportException

/*
 * @since   Oct. 12, 2021
 * @version Oct. 12, 2021
 * @author  ASAMI, Tomoharu
 */
class ConclusionException(conclusion: Conclusion) extends GoldenportException(conclusion.message.en) {
}
