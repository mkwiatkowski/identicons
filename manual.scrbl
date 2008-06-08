#lang scribble/doc
@(require scribble/manual
          scribble/eval
          (for-label scheme/gui)
          (for-label "identicons.ss"))

@title{Identicons library}

@defmodule*/no-declare[((planet "identicons.ss" ("mk" "identicons.plt" 1 0)))]
@declare-exporting["identicons.ss"]

This library is a PLT Scheme implementation of a graphical identification
scheme created by Don Park, first described in his blog post
@(link "http://www.docuverse.com/blog/donpark/2007/01/18/visual-security-9-block-ip-identification" "Visual Security: 9-block IP Identification").
It is capable of generating identicons from 32-bit numbers,
using the same generation scheme as the one used by Don Park.

A standard way to use the library is to create an identicon using
the @scheme[identicon%] constructor and save it to file using its
@method[identicon% save-to-file] method. You can also take the generated
identicon for further manipulation by converting it to a bitmap using
the @method[identicon% on-bitmap] method. For DrScheme interaction
window love, check out @method[identicon% as-image-snip] method.

@section[#:tag "identicon-object"]{identicon% class}

To make an identicon from a single 32-bit value, pass it as an argument during
object creation:

@defclass[identicon% object% ()]{

@defconstructor/make[((seed integer?))]{
Creates a new identicon using bits from the given seed.}

@defmethod[(draw [dc (is-a?/c dc<%>)])
           void?]{
Draw the identicon on given dc using all available space. Since
identicons are square, that will be @scheme[dc]'s width or height,
whichever is smaller.

@bold{Note}: @scheme[dc] will be cleared before drawing and its properties will be
altered.}

@defmethod[(on-bitmap [size integer?])
           (is-a?/c bitmap%)]{
Return a bitmap of given size with this identicon drawn on it.}

@defmethod[(as-image-snip [size integer?])
           (is-a?/c image-snip%)]{
Return an @scheme[image-snip%] of given size with this identicon inside.}

@defmethod[(save-to-file [file-path path-string?] [size integer?])
           void?]{
Save this identicon of the given size under the file-path.}

@defmethod[(display-in-frame [size integer?])
           void?]{
Display this identicon in a square frame of a given size.}
}

@section[#:tag "functions"]{Top-level functions}

@defproc[(random-identicon)
         (is-a?/c identicon%)]{
Create an identicon using a random seed value.}

@defproc[(save-random-identicon)
         path-string?]{
Save a random identicon under a filename derived from its seed value.
Procedure returns the generated filename.}

@defproc[(display-random-identicon)
         void?]{
Display a random identicon in a new 300px frame.}

@section[#:tag "example"]{Example}

As an example of how to generate and combine a number of identicons, analyze the
following function @scheme[identicons-grid] that generates a grid of NxN identicons
and saves it to @tt{grid.png} file.

@schemeblock[
(define (identicons-grid identicon-size identicons-number)
  (let* ((grid-size (* identicon-size identicons-number))
         (grid-bitmap (make-object bitmap% grid-size grid-size))
         (grid-dc (new bitmap-dc% (bitmap grid-bitmap))))
    (for* ((x (in-range identicons-number))
           (y (in-range identicons-number)))
          (let* ((identicon (random-identicon))
                 (bitmap (send identicon on-bitmap identicon-size)))
            (send grid-dc draw-bitmap bitmap (* x identicon-size)
                                             (* y identicon-size))))
    (send grid-bitmap save-file "grid.png" 'png)))
]
