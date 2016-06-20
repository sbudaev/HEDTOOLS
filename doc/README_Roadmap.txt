This directory contains tools and modules for the next generation of HED

Roadmap = TODO:

 1. split the source code,
 2. identify legacy/unused parts - stripe them off
 3. stripe all write from the source code, write will use centralised subroutine
    with CSV (other format) format.
 4. add automated runtime execution checkers and "chart" generator (fra flibs???)
 5. add preprocessor (from flibs?? simple!).
 5. add unitary exception handling routines (from flibs??)
 6. add csv output module(s) (from csv_libs)
 7. add string manipulation routines (flibs??)
 8. add automatic documentation generator (doctran?? or simpler??)
 9. add unit test framework (fortranxunit or simpler???)
10. add interfaces for R and Matlab (???)


On 04/11/15 10:07, Sergey Budaev wrote:
> Judy,
>
> Not yet. Actually my plans are to split the code into the pieces that
> are doing useful stuff (there are a lot of heritable and unused lines),
> stripe everything write (output), and then produce a series of modules
> that could be used and quickly changed in various projects/models/etc.
> Ideally, it should be something like lego bricks that we could
> assemble/modify for different models.
>
> At this stage I thought to include the mood modulation, and perhaps
> other kinds like "arousal." If the code is modularised and easy to work
> with, we can produce several models with different modulation (e.g. with
> and without) and compare the output directly.
>
> The code is monolithic and very difficult to work with, currently. And I
> do not know it very well.
>
> But your idea is certainly interesting.
>
> As to this moment, I think you should proceed on the current model as
> splitting/assembling/editing the code to a modular form requires time
> and will result in bugs (and most probably more or less tedious
> debugging). And you are in a rush :( I have not even started the
> modularisation process yet.
>
> The time scale when I am ready to discuss this in details is perhaps a
> couple of weeks. I hope this time I could split and reassemble the basic
> model. If everythin is modular, it could be not too difficult to merge
> your squirrel specificity and then introduce moods and speeds.
>
> Sergey
>


