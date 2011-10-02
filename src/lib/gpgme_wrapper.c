/* GPGME/G : GPGME-with-Guile
 *
 * A Guile binding to the GPGME library
 * Wrapper library for some of the unfortunate GPGME peculiarities
 *
 * Copyright © 2011 Atom X
 *
 * This library is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implide warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

#include <libguile.h>
#include <gpgme.h>

/* The GPGME function `gpgme_get_engine_info' does not provide a very
 * _clean_ interface: it returns an integer, which should be
 * interpretted as an error code, and then _modifies_ the
 * gpgme_engine_info_t argument passed to it.  This doesn't jive well
 * with the Scheme-way of doing things, thus this wrapper has been
 * devised.
 *
 * We pass in a pointer to a function whose prototype is
 * (f integer) -> symbol (though it is seen as f(SCM) -> SCM in C),
 * the resulting symbol being the key to a thrown error if something
 * goes wrong with the internal call to `gpgme_get_engine_info'.
 *
 * This is all a very convoluted way to sanitize a C function.  :-(
 */

gpgme_engine_info_t engine_info;

gpgme_engine_info_t
retrieve_engine_info (SCM (*errno_to_symbol) (SCM))
{
  gpgme_error_t        err;

  err = gpgme_get_engine_info (&engine_info);

  if (!err) {
    return engine_info;
  } else {
    scm_error_scm (errno_to_symbol (scm_from_unsigned_integer ((scm_t_uintmax) err)),
		   scm_from_locale_string ("retrieve_engine_info"),
		   scm_from_locale_string ("An error was encountered when trying to retrieve the crytography engine information"),
		   scm_list_n (SCM_UNDEFINED), /* An empty list */
		   SCM_BOOL_F);
  }
}
