fluidPage(
  title = '"Dexter" TV series text-mining analysis',
  
  tags$div(
    style = "text-align: center;",
    tags$img(src = "prefix/images/dexter-2006-logo.png", height = "150px")
  ),
  
  tags$h2("TEXT-MINING ANALYSIS", style = "text-align: center; font-size: 26px; font-weight: bold; margin-top: 5px;
          text-shadow: 1px 1px #FF0000; color: black;"),
  
  tags$div(class = "container",
           tags$button(id = "disclaimer_button", class = "info-button",
                       tags$img(src = "prefix/images/info3.svg", height = "30px", width = "32px"))
  ),
  
  tags$div(id = "disclaimer_modal", class = "modal",
           tags$div(class = "modal-content",
                    tags$span(id = "close_modal", class = "close", HTML("&times;")),
                    tags$h3("DISCLAIMER", style = "font-family: serif; text-align: center; letter-spacing: 2px;"),
                    tags$div("This text-mining analysis has been created with the use of scripts from \"Dexter\" series, 
                              but only the ones from season 1 with the addition of the 2nd episode of season 2 script - 
                              just to give it more variety.",
                             style = "text-align: center; font-family: serif; margin: auto; font-size: 18px;")
           )
  ),
  
  tags$div(class = "musicOn",
           tags$audio(id = "audio", src = "prefix/mp3/dexter-main-title-ost.mp3",
                      type = "audio/mp3", autoplay = NA, loop = TRUE,
                      style = "display:none;")
  ),
  tags$div(class = "container",
           tags$button(id = "toggle-music", class = "music-button",
                       tags$img(id = 'note-icon', src = "prefix/images/note.svg", width = "30px", height = "32px"))
  ),
  
  tags$style(HTML("
    .music-button {
      background: none;
      border: none;
      padding: 0;
      cursor: pointer;
      position: fixed;
      right: 25px;
      top: 15px;
      touch-action: manipulation;
      z-index: 1000;
    }

    #note-icon {
      transition: transform 0.4s ease-in-out, opacity 0.4s ease-in-out, filter 0.4s ease-in-out;
    }

    #note-icon.paused {
      opacity: 0.4;
      transform: scale(0.95);
    }

    #note-icon.playing {
      opacity: 1;
    }
    
    #note-icon:hover {
      opacity: 1;
      transform: scale(1.1);
    }
    
    .info-button {
    background: none;
    border: none;
    padding: 0;
    cursor: pointer;
    position: fixed;
    right: 65px;
    top: 15px;
    touch-action: manipulation;
    z-index: 1000;
    opacity: 0.4;
    transition: transform 0.4s ease-in-out, opacity 0.4s ease-in-out;
  }

  .info-button:hover {
    opacity: 1;
    transform: scale(1.1);
  }
    
  .disclaimer-button {
    background: none;
    border: none;
    color: black;
    opacity: 0.6;
    font-family: serif;
    font-size: 16px;
    letter-spacing: 1px;
    cursor: pointer;
    position: absolute;
    right: 165px;
    top: 15px;
    z-index: 1000;
    transition: opacity 0.3s ease-in-out;
  }

  .disclaimer-button:hover {
    opacity: 1;
  }

  .modal {
    display: none;
    position: fixed;
    z-index: 2000;
    left: 0;
    top: 0;
    width: 100%;
    height: 100%;
    overflow: auto;
    background-color: rgba(0,0,0,0.4);
  }

  .modal-content {
    background-color: #fff;
    margin: 15% auto;
    padding: 30px;
    width: 600px;
    border-radius: 10px;
    font-family: serif;
  }

  .close {
    color: #444;
    float: right;
    font-size: 40px;
    font-weight: bold;
    cursor: pointer;
  }

  .close:hover,
  .close:focus {
    color: black;
    text-decoration: none;
  }
  ")),
  
  tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      const button = document.getElementById('toggle-music');
      const audio = document.getElementById('audio');
      const icon = document.getElementById('note-icon');
      
      audio.volume = 0.15;
      
      function updateIcon() {
        if (audio.paused) {
          icon.classList.remove('playing');
          icon.classList.add('paused');
        } else {
          icon.classList.remove('paused');
          icon.classList.add('playing');
        }
      }
      
      updateIcon();
      
      button.addEventListener('click', function() {
        if (audio.paused) {
          audio.play();
        } else {
          audio.pause();
        }
      });
      
      audio.addEventListener('play', updateIcon);
      audio.addEventListener('pause', updateIcon);
    });
    
      document.addEventListener('DOMContentLoaded', function() {
    var modal = document.getElementById('disclaimer_modal');
    var btn = document.getElementById('disclaimer_button');
    var span = document.getElementById('close_modal');

    btn.onclick = function() {
      modal.style.display = 'block';
    }

    span.onclick = function() {
      modal.style.display = 'none';
    }

    window.onclick = function(event) {
      if (event.target == modal) {
        modal.style.display = 'none';
      }
    }
  });
  ")),

  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3('Top 100 most frequent words in "Dexter"'),
      div(style = "display: inline-block; font-weight: bold;",
          wordcloud2Output("wordcloud_no2", width = "1200px", height = "575px")),
      tags$h4('The size of each word is proportional to its frequency of occurence, with a power transformation (exponent 0.3)
              applied to reduce the dominance of the most common words and to improve overall readability. We can see how the name
              of the main character: Dexter, is easily the most frequent term with a count of 3608 occurences (it will also be noticeable in latter 
              parts of the analysis). His sister, Debra, takes the 3rd place with 930 occurences ("not" got 2nd place, but negations
              will be more important later).', 
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3('Top 200 most frequent words in "Dexter"'),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/wordcloud2mono.png", width = "395px", height = "381px")),
      tags$h4("This word cloud is a little bit different, but overall it serves the same purpose - here, it is simply for the comparison
      of the two and more variety. The only differences are, that it has total 200 words (100 more than in the 1st 
              wordcloud), and it's created with an older version of the wordcloud2 package: wordcloud.",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3('Word associations with "biney" - TF-IDF contextual analysis'),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/assoc-plot.png", width = "650px", height = "575px")),
      tags$h4("Here, the plot presents words that are most strongly associated with the word", tags$b('biney'),
              'based on their co-occurence within 100-token text fragments, which is calculated using the TF-IDF weighting 
              scheme. The horizontal axis shows the strength of the correlation (Pearson coefficient), reversed for clarity.
              Each dot represents a word with at least a 0.3 correlation to "biney".', br(), br(),
              'For example the word', tags$b("brother"), 'has a 0.31 correlation to', tags$b("biney"), 
              ', because biney (diminutive for Brian) was a brother to Dexter - which was sometimes mentioned.',
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3('Top 9 most frequent words per script - comparative bar plots'),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/incidence-plot.png", width = "1050px", height = "900px")),
      tags$h4('This plot enables a side-by-side comparison of dominant vocabulary across different "Dexter" scripts.', br(), br(), 
              'As mentioned earlier, the word', tags$b('dexter'), 'is the most frequent word in all the documents - but if we look closely,
              we can see a lot of interesting words in given scripts. For example, in the 8th episode of season 1, we can see words',
              tags$b('meridian'), 'and', tags$b('shrink') ,'both connected to Emmett Meridian, a character introduced in this episode.
              Emmett was a psychiatrist and he was mentioned many times throughout the episode, because he was both', "Dexter's",
              'psychiatrist and target - a target to ', tags$span(('kill.'), style = 'color: red;'),
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3('Shared words across all episodes - commonality cloud'),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/commonality-cloud.png", width = "473px", height = "476px")),
      tags$h4('Next, a commonality word cloud which shows most frequent words that occured at least once in all of the episodes.
              As always, the most frequent common word is', tags$b('dexter'), ', but besides that, we can also see many more 
              important words, as for example the names of major characters like',
              tags$b('debra'), ', ', tags$b('doakes'), ', ', tags$b('laguerta'), 'or', tags$b('batista'), '.',
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3('Distinctive words across all episodes - comparison cloud'),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/comparison-cloud.png", width = "1026px", height = "914px")),
      tags$h4("Another word cloud, but in this one we don't see words that episodes have in common, but on the contrary: we see
              the differences - most distinct terms in each script.", br(), br(), 
              "But here is an interesting question: why is the word", tags$b('dexter'), 'only in the 2nd episode of season 2?
              This word has been the most frequent in all of the episodes and because of that it cannot be a distinct word in all
              of them - it just happens so, that in this script it was repeated a lot more than in the other ones.',
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3('Frequence comparison of 25 most frequent common words in "Dexter" 1x01 and 1x02'),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/pyramid-plot-red.png", width = "730px", height = "514px")),
      tags$h4('This pyramid plot presents aforementioned 25 most frequent common words in the 1st and 2nd episode of "Dexter" season 1.
              It helps us notice how some plots or characters may occur, change in frequency of their appreance or do not appear at all.',
              br(), br(),
              'For example, the word ', tags$b('batista'), "is mentioned only 3 times in 1x01, but in 1x02 it occurs almost 100 times.
              In the 1st episode Angel Batista is reffered to by his name, Angel, and he doesn't appear in many scenes. In the 2nd
              episode, the character has a lot more dialogues and is described more often. In the scripts, Angel Batista is usually
              referred to as just Batista. It's just that the 1st episode was barely focused on him.",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3('Top 12 words by positive and negative sentiments - frequency comparison (bing lexicon)'),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/frequent-words-sentiment-bing.png", width = "986px", height = "553px")),
      tags$h4("This bar plot highlights the top 12 most frequently occuring words for each sentiment category - positive and negative.
              We can compare the count of both positive and negative words, we can see if there is more of these or those and we can tell
              which ones are the most frequent.", br(), br(),
              "In this case, top positive words are", tags$b('like'), ',', tags$b('good'), ',', tags$b('right'), 'while the top negative words are',
              tags$b('fuck'), ',', tags$b('killer'), 'and', tags$b('kill'), '- in both groups all 3 terms are objectively known for 
              being respectively positive and negative. Also, the negative group suits the theme of the show perfectly, especially
              the words', tags$b('killer'), 'or', tags$b('kill'), '. The word', tags$b('fuck'), "is so common mostly because of 
              Debra Morgan's character, who, to put it lightly, swears all the time (the word", tags$b('shit'), "is also up there).",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3('Top 12 words by positive and negative sentiments - frequency comparison (nrc lexicon)'),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/frequent-words-sentiment-nrc.png", width = "996px", height = "516px")),
      tags$h4("The same type of plot as the one above, but here a different lexicon is used (nrc), so the results are different.", br(), br(),
              "It's interesting to note that the term", tags$b('harry'), "is considered the most frequent negative word here, because of
              it's verb definition of constantly harassing somebody, but what the function doesn't understand is that it's also
              a name of a character Harry Morgan - Dexter's beloved foster father. As in for the positive words, ", 
              tags$b('good'), 'is the most frequent and', tags$b('pull'), 'is the 2nd (pull as in attract or hit on somebody).',
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3('Top 12 words by constraining, litigious and uncertainty sentiments - frequency comparison (Loughran lexicon)'),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/frequent-words-emotions-loughran.png", width = "996px", height = "539px")),
      tags$h4("Here, we can see top 12 words most strongly associated with various non-polar emotional categories (excluding
              positive and negative) from the Loughran sentiment lexicon. Each group represents a different category - such as
              constraining, litigious or uncertainty - with the bars indicating word frequency.", br(), br(),
              'Why "non-polar"? What is the difference between this sentiment and positive/negative one? The difference is, that
              polarity gives a general emotion direction: good or bad - while non-polar emotions indicate a specific type of emotion
              or tone: does the text cause concern, uncertainty, threat, constraint, formality etc.', br(), br(),
              "In this case, it's very apparent for some words to be grouped correctly. For example, terms like",
              tags$b('lawyer'), 'or', tags$b('court'), 'are objectively considered litigious. 
              Words like', tags$b('may'), 'or', tags$b('maybe'),"raise uncertainty, so they're in the", 
              tags$span("uncertainty", style = 'font-weight: bold; color: #619cff;'),
              "group. Also, the words", tags$b('commit'), 'from the', 
              tags$span('constraining', style = 'font-weight: bold; color: #f8766d;'), 
              'group, and', tags$b('crime'), 'from the', 
              tags$span('litigious', style = 'font-weight: bold; color: #00ba38;'), 'group, agree with the series main theme a lot, 
              because, well, "Dexter" is about a serial killer who is', tags$b('committing crimes'), '.',
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3('Top 12 words by constraining, litigious and uncertainty sentiments - frequency comparison heatmap (Loughran lexicon)'),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/frequent-words-emotions-loughran-heatmap-red.png", width = "470px", height = "734px")),
      tags$h4("This plot represents the same aspect of the analysis, as the plot above, with the sole difference of being
              a different type of a plot: a heatmap. Mainly made for better comparison readibility and variety. For some viewers,
              it might be easier to notice a disparity in frequencies of the words in given groups.",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3('Words preceded by "not" with the highest sentiment impact (AFINN lexicon)'),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/bigrams-influence-word-sentiment.png", width = "820px", height = "634px")),
      tags$h4("Here, we have the 20 most influential bigrams where the word", tags$b('not'), "precedes another sentiment-bearing
              word (e.g. not good, not happy). The bars show the product of word frequency and AFINN sentiment score, reflecting
              each word's total sentiment contribution in this negated context. Positive and negative influences are shown in
              opposite directions and different colors (", tags$span('green', style = 'color: #00ba38;'), " - positive",
              tags$span('red', style = 'color: #f8766d;'), "- negative).", br(), br(),
              "Negative negated words like: ", tags$b('fuck'), 'or', tags$b('kill'), "have such high sentiment score, because
              they are combined with don't (do not), which makes bigrams like", tags$b("not fuck"), 'and', tags$b("not kill"),
              'which are derived from small sentences like:', tags$b("don't fuck with me"), 'or', tags$b("please don't kill me"),
              '- these ones fit perfectly into "Dexter" theme and occur often.', br(), br(),
              "A little disclaimer: one could make a case that", tags$b("don't"), 'and', tags$b('do not'), 
              "aren't considered the same and one would be right, but while cleaning the text from the scripts, I made sure that
              word endings like", tags$b("n't"), "are considered as", tags$b('not'), '.',
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3('Words preceded by negations with the highest sentiment impact (AFINN lexicon)'),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/words-distorting-sentiment.png", width = "813px", height = "649px")),
      tags$h4("This one is pretty straightforward, as it covers the same aspect of the analysis as the previous one, but with
              words preceded by different negations (",tags$b('not'), "is also there).", br(), br(),
              "A great example is the bigram", tags$b('without warn'), ", which could create a sentence: ", tags$b("shoot without warning"),
              "- for police officers. Scenes with MMPD (Miami Metro Police Department) officers shooting happen a lot in the series,
              which would explain this sentence.",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3('Sentence-level polarity distribution across all episodes'),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/polarity-sentiment-test-plot.png", width = "1054px", height = "1048px")),
      tags$h4("This plot displays the distribution of sentence-level polarity scores for the first 500 sentences, grouped by
              episode (script). Sentence-level polarity means that polarity value is measured for every sentence individually.
              Then, it counts the average polarity for a document, using all the sentence-level polarity scores.", br(), br(),
              "These polarity scores tell us a lot about the tone of an episode. Positive values mean positive sentiment and
              negative values mean negative sentiment.", 'In this case, all polarity scores are negative, ranging from roughly',
              tags$b('-0.4'), 'to', tags$b('-1.7'), 'meaning that every script has a negative sentiment.', br(), br(),
              'One example worth mentioning is the difference in polarity values between the 1st episode and the 10th - 
              approximately -0.4 and -1.7, respectively.',
              'The first episode serves as an introduction to Dexter. While it is still grim, it acts as a warm-up: 
              we get an overview of how Dexter lives, what he does for a living, who his friends and family are, and,
              most importantly, we learn that Dexter is a serial killer who hunts other killers.',
              "In comparison, episode ten shows Dexter being called to investigate a crime scene, a room completely covered 
              in blood. This gruesome scene triggers a deeply suppressed memory: a childhood trauma involving a similar 
              homicide, in a room that looked almost exactly like the one he's now standing in. 
              (The details make it more unsettling - but no spoilers here.)",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3('Positive and negative sentiment comparison word cloud'),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/sentiment-polarity-comparison-cloud.png", width = "730px", height = "598px")),
      tags$h4("This comparison word cloud presents the most significant terms (based on TF-IDF weighting) from positively
              and negatively scored sentences. Words in bold indicate high importance within their sentiment category.", br(), br(),
              "Interestingly, the word", tags$b('meridian'), "(last name of a character mentioned before - Emmett Meridian) from viewer's
              point of view, shold be negative because the character with this name was simply a bad guy - but what he said sounded positive,
              and that's why the function put this word into the positive category.", br(), br(),
              "On the other hand, in the negative group, we have the word", tags$b('lundy'), "which is the last name of the 
              character Special Agent Frank Lundy, who was called to Miami to investigate the Bay Harbor Butcher killings - Dexter's
              victims - which Dexter wasn't really happy about, ergo", tags$b('lundy'), "is in the negative category.",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3('10 most characteristic words associated with each latent topic in the scripts (LDA model)'),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/lda-topics-plot.png", width = "1221px", height = "439px")),
      tags$h4("Here, the chart displays top 10 words most strongly associated with each topic identified by a Latent
              Dirichlet Allocation (LDA) model applied to the scripts. Every document is a mix of latent topics and every topic
              is a probability distribution (beta) of words (words that are typical for given topic). The model tries to uncover topics,
              whose existence hasn't been specified in advance - hence the term", '"latent".', br(), br(),
              tags$b("Season 1 spoilers down below"), br(), br(),
              "1st topic might be connected to Dexter's character and major plot points because of keywords like",
              tags$b('rudy'), ',', tags$b('brian'), 'or', tags$b('paul'), 
              "- characters directly connected to Dexter (while rudy and brian being technically the same character, 
              but of course R doesn't know that). Besides that, there are also last two words",
              tags$b('free'), 'and', tags$b('bear'), 
              "- bear being the infinitive of born and combine that with free and we have the title of the final episode of season 1:",
              tags$b('"Born Free"'), '.',
              "Not only does this indicate that this topic is connected to aforementioned major plot points, but also 
              it's connected to the final episodes of 1st season (where Dexter gets to know that Rudy is Brian etc.)",
              br(), br(),
              "2nd topic's two keywords of the highest values are",
              tags$b('sergeant'), 'and', tags$b('masuka'), 
              "- from just these two it's hard to say 
              what's the topic about, but words like",
              tags$b('chino'), ',', tags$b('deb'), 'or', tags$b('lundy'), 
              "could again connect the topic directly to Dexter, but
              more on the side of his actual work in MMPD (Miami Metro Police Department), not what he does in his own time.", br(), br(),
              "The 3rd topic is probably about police work and psychiatry, since we have names of the characters connected to a major MMPD case,
              and a psychiatrist. The word", tags$b('tucci'), "is a last name of", tags$b('Tony Tucci'), ", a character who was 
              abducted and the police was trying to find him. Secondly, we have words like", tags$b('sergeant'), ',', 
              tags$b('meridian'), 'and', tags$b('shrink'), 'which connect the topic directly to both police and psychiatry 
              (the only sergeant in "Dexter" season 1 is MMPD Sergeant James Doakes and Emmett Meridian was already mentioned 
              a couple of times, but as a remainder: he is a psychiatrist).',
              style = 'text-align: left; font-style: italic;'),
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3('Most distinctive words between topic 1 and topic 2 (based on log-ratio of word probabilities)'),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/beta-difference-sentiment-plot.png", width = "1003px", height = "672px")),
      tags$h4("This plot highlights the 20 words with the greatest divergence in association between Topic no. 1 and Topic no. 2,
              based on log₂ ratio of their LDA beta probabilities. A positive log-ratio indicates a stronger association with Topic 2,
              while a negative value favors Topic 1. This chart helps identify which words truly distinguish these topics.",
              br(), br(),
              "For example, we know from the previous plot, that Topic 1 is connected to major plot points, Dexter and Brian - here, the word",
              tags$b('monique'), "could connect it even more to Brian, because Monique was a character that gave police clues
              which helped them solve a case revolving around Brian.",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3('Cluster quality estimate - Dunn index'),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/cluster-quality-estimate-plot.png", width = "730px", height = "598px")),
      tags$h4("The red line shows the Dunn index values for hierarchical clustering solutions with 2 to 10 clusters, 
              using Euclidean distances and Ward’s linkage method. The Dunn index is a measure of clustering quality - higher 
              values indicate better-defined, well-separated clusters.", br(), br(),
              "In this case, with 10 documents, optimal number of clusters amounting to 2 is plausible and should work relatively
              well.", br(), br(),
              "Although, as mentioned above, it is better to have more clusters, but sometimes you gotta work with what 
              you've got and it's no different in this case - I wasn't able to find more Dexter season 1 scripts. There are only
              3 more, 1 from season 2 and 2 from season 3 - seasons that aren't the point of this analysis (2x02 was added for little
              variety, especially in LDA topics).",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3("Hierarchic document clustering dendrogram (Ward's method) with a 2-cluster solution"),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/dendrogram-k2.png", width = "773px", height = "533px")),
      tags$h4("This dendrogram visualizes the hierarchical structure of the dataset using Ward's clustering method,
              which minimizes total within-cluster variance (the smaller the value, the better is the coherence of the group -
              and that's what Ward's method tries to achieve, small values during the process of connecting the groups).", br(), br(),
              "Each branch represents a merging of the scripts, based on their lexical similarity.
              The two outlined rectangles indicate the partitioning of the tree into 2 distinct clusters. These two clusters
              likely represent any differences in thematic or emotional content between segments.", br(), br(),
              'The simplest way to explain the difference between the two clusters is to look at which scripts were grouped
              together. The left cluster contains the later episodes, ranging from episode 8 to the final episode 12, while
              the right cluster includes the early episodes, from episode 1 to episode 6. Thematic content differs between these
              groups, as the more you get into season 1 of "Dexter", the darker the narrative becomes.', 
              "Apparently, using Ward's method, the algorithm identified episode 6 as the last point where the plot is relatively 
              not that disturbing yet, making the 8th episode the point where the series starts to feel noticeably more intense
              and unhinged.", br(), br(),
              "Also, in the right cluster we have 2x02 episode, so technically it's after the latter episodes of season 1, but
              it still is a episode that commences season 2 so my point still stands.",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3('Cluster groups'),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/cluster-groups.png", width = "774px", height = "598px")),
      tags$h4("Here is a very simple plot showcasing which documents were put into which cluster.",
              tags$span('Green', style = 'color: #00ba38;'), " - cluster 1", ' | ',
              tags$span('Red', style = 'color: #f8766d;'), "- cluster 2.",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3("Dunn index evaluation for k-means clustering - worst case (k = 2)"),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/kmeans-dunn-worst-case.png", width = "774px", height = "598px")),
      tags$h4("This line plot presents the Dunn index values for k-means clustering across a range of cluster counts
              (from 2 to 9), using a normalized document-term matrix and Euclidean distance.", br(), br(),
              "The Dunn index serves as a quality metric for clustering by balancing inter-cluster separation and intra-cluster 
              compactness - higher values indicate more distinct and tighter clusters.", br(), br(),
              "Inter-cluster separation - separating clusters, we want them far away from each other,", br(),
              "Intra-cluster compactness - compactness within the clusters, we want the elements of a given 
              cluster to be close to each other.", br(), 
              "(inter - in between) (intra - within)",
              br(), br(),
              "The dashed red line highlights the cluster count (k) that yields the lowest Dunn index - in this case there are both
              low cluster count (2) and low Dunn index.",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3("Dunn index evaluation for k-means clustering - best case (k = 9)"),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/kmeans-dunn-best-case.png", width = "774px", height = "598px")),
      tags$h4("But here, we have technically the best case, where we get the highest possible number of clusters (9) with the highest
              possible Dunn index. But, it doesn't make much sense and is suboptimal, because we don't want 9 clusters for 10",
              'documents. This might be a case of overclustering, where the groups are just "mathematical", but',
              "don't bring much semantical value - while it technically might work, there is no point in analysing 9 clusters
              of the total 10 documents. That is why the next plot might be helpful.",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3("Average silhouette score for k-means clustering - best case (k = 2)"),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/kmeans-silhouette-best-case.png", width = "774px", height = "598px")),
      tags$h4("This plot displays the average silhouette score for k-means clustering across different numbers of clusters
              (again k = 2 to 9), again using Euclidean distance on a normalized document-term matrix.",
              "The difference between Dunn index and average silhouette score is that the latter measures how well each data
              point fits within its assigned cluster compared to other clusters. Scores closer to 1 indicate well-separated
              clusters, negative scores indicate wrongly assigned data to clusters. Scores closer to 0, such as 0.08 here,
              suggest that the clustering structure is weak - documents are not clearly assignable to distinct groups and may
              lie near decision boundaries - meaning that the points (fragments of the scripts) are not clearly assigned to any
              cluster, because they are located near the boundaries between different clusters.", br(), br(),
              "While the Dunn index achieved its maximum at k = 9, this result is a mathematical artifact caused by nearly
              one document (script) per cluster. In contrast, the average silhouette score peaked at k = 2, suggesting that
              2 clusters offer a more coherent separation of the data. Given the small sample size (10 documents), and the
              semantic nature of text clustering, I prioritized the silhouette result to avoid overfitting and ensure
              interpretability.", br(), br(),
              "(overfitting - fitting the data too closely, which harms generalisation.)",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3("2D visualization of k-means clustering using MDS (k = 2)"),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/cluster-grouping-k2-mds.png", width = "774px", height = "598px")),
      tags$h4("Next up, here is a scatter plot which visualizes the result of k-means clustering with k = 2 applied to the normalized
              document-term matrix, projected into two dimensions using MDS (Multidimensional Scaling).", br(), br(),
              "Each point represents a script, and colors indicate its assigned cluster. The two axes,", tags$b('Dim 1'),
              'and', tags$b('Dim 2'), ", reflect the major dimensions of variation in the data based on Euclidean distances
              between the documents.", br(), br(),
              "In this case, we divided the documents into 2 clusters and chosen so in this plot. The points aren't grouped
              perfectly, but also it seems that there is a clear separation between both clusters. This doesn't indicate much,
              in the case of series like", '"Dexter"', ", this could reveal some differences in between the scripts in given cluster,
              but I have already covered that difference probably being the change of the tone into a darker one going into episode 8
              and later (assumption why X,Y,Z scripts were put into X cluster and so on).",
      style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3("2D visualization of k-means clustering using MDS (k = 3)"),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/cluster-grouping-k3-mds.png", width = "774px", height = "598px")),
      tags$h4("This plot covers the same aspect of the analysis as the previous one, but with the difference of the points
              being divided into 3 clusters instead of 2 - simply experimenting.", br(), br(),
              "With 3 clusters, there is a visible difference in the separation of the clusters and it seems like the groups
              are separated better, but technically there is still a lot of space within the respective groups so this solution
              doesn't seem like a much better one.",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3("PCA-based visualisation of k-means clustering (k = 2)"),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/pca-cluster-k2.png", width = "1011px", height = "547px")),
      tags$h4("The first plot shows the result of k-means clustering with k = 2 applied to the original
              document-term matrix (dtm3_m), visualized using PCA (Principal Component Analysis). Each point is a script segment, 
              colored by its assigned cluster. The shape of the clusters suggests how well-separated the themes or language patterns 
              are across the full vocabulary of the scripts.",
              "As it was in the previous plot, here it is very similar: the groups are poorly separated in between each other,
              but cluster 1 seems to have a couple of points grouped closely - potentially similar language patterns in these segments.",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3("PCA-based visualisation of k-means clustering (k = 3)"),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/pca-cluster-k3.png", width = "995px", height = "508px")),
      tags$h4("Same as previous plot but with the division of the points into 3 clusters instead of 2.", br(), br(),
              "On one hand it seems better because clusters 1 and 2 seem more compact but the separation is still poor.
              Cluster 3 isn't separated from other clusters aswell and to top it off it's not compact at all.",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3("PCA-based visualisation of k-means clustering (no sparse terms, k = 2)"),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/pca-cluster-no-sparse-terms-k2.png", width = "774px", height = "598px")),
      tags$h4("Another PCA-based visualisation of k-means clustering, but in this case, there are no sparse terms in the
              document-term matrix.", br(), br(),
              "Clusters are separated, the script segments aren't compact well enough, but even then this is a relatively better
              solution.",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3("PCA-based visualisation of k-means clustering (no sparse terms, normalized, k = 2)"),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/pca-cluster-no-sparse-terms-k2-normalized.png", width = "774px", height = "598px")),
      tags$h4("In this final PCA visualisation there are no sparse terms and the document-term matrix is normalised.", br(), br(),
              "As it appears, the clusters aren't separated and only cluster 2 is compact, ergo the solution before the
              normalisation was better.",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3("Spherical k-means clustering of script segments with cluster distribution"),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/skmeans-clusters-barplot.png", width = "774px", height = "598px")),
      tags$h4("This bar plot is pretty straightforward, it presents the distribution of the scripts across two spherical
              clusters. Also, it reveals that the scripts are not evenly split and one cluster dominates slightly.",
              br(), br(),
              "As mentioned before, specific scripts were grouped into particular clusters potentially based on the overall", 
              '"darkness level"', "of each script's theme. Cluster 1 has more scripts, so we can assume that the 
              earlier episodes of", '"Dexter", ', "were less grim than the episodes later, thus there is more episodes where 
              the theme isn't greatly disturbing - ", tags$span("just a tad.", style = 'color: red;'),
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3("MDS (Multidimensional Scaling) plot"),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/skmeans-clusters-2d-k2.png", width = "774px", height = "598px")),
      tags$h4("This plot visualizes the script segments in 2D space based on their pairwise distances. Coloring by spherical 
              cluster assignment allows us to visually assess the cohesion and separation of clusters.", br(), br(),
              "The results suggest that the two clusters form reasonably distinct groups, though some overlap may indicate
              that specific scripts were very similar in their respective themes.",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3("Silhouette plot for soft k-means clustering (k = 2, m = 1.2)"),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/skmeans-silhouette-plot.png", width = "774px", height = "598px")),
      tags$h4("This silhouette plot shows how well each document fits into one of the two clusters formed by fuzzy k-means
              clustering. Each bar represents the silhouette score of a single script, which reflects how similar it is
              to its own cluster compared to the nearest alternative cluster.", br(), 
              "(soft/fuzzy clustering - a form of clustering in which each data point can belong to more than one cluster)",
              br(), br(),
              "In this case, using", tags$b('k = 2'), "and fuzziness parameter", tags$b("m = 1.2"), ", the silhouette values
              are generally close to 0, indicating that the separation between the two clusters is weak. Some scripts have low
              or the episode 2's script having a negative score, meaning they lie near the decision boundary or may be 
              better suited to another cluster. But overall, even though the values are relatively low, some values come close
              to 0.4 like the episode 10 and 11th's scripts, so these two seem to have been grouped correctly.",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3("Term comparison cloud of spherical clusters"),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/k2-clusters-comparison-cloud.png", width = "774px", height = "598px")),
      tags$h4("This comparison cloud visualises the most distinguishing terms for each spherical cluster derived from the
              script segments. The words shown are the", tags$b('prototype terms'), "- that is, the terms most representative
              of each cluster.", br(), br(),
              "In cluster 1 there are words grouped accordingly to the plots in the later episodes (8 to 12) like",
              tags$b('rudy'), ',', tags$b('brian'), 'or', tags$b('meridian'), '. In cluster 2 words like',
              tags$b('tucci'), ',', tags$b('crocodile'), 'or', tags$b('castillos'), 'are also grouped correctly.',
              "Knowing which characters took a part in a plot and what happened in specific episodes, transfers to knowing what 
              is a given cluster about.",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3("Silhouette plot for k-medoids clustering"),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/k-medoid-clustering-avg-silhouette.png", width = "774px", height = "598px")),
      tags$h4("This silhouette plot evaluates the quality of k-medoids clustering on the normalised document-term matrix
              (dtm3_m) using cosine-based dissimilarity. Each bar represents an individual script (episode), and its length
              indicates the silhouette coefficient, a measure of how well a document fits within its assigned cluster to others.
              Values close to 1 suggest strong cohesion within clusters and clear separation from others.", br(), br(),
              "Here, the structure shows that the selected number of clusters (chosen automatically as optimal by the pamk function)
              forms relatively well-separated and coherent groups. Although, the scripts 3 and 10 have very low silhouette
              coefficient, so they probably differ in theme/tone.",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
  
  div(style = "text-align: center; font-family: serif;",
      tags$h3("Silhouette plot for k-medoids clustering based on Calinski-Harabasz criterion"),
      div(style = "display: inline-block; margin-top: 20px; font-weight: bold;",
          tags$img(src = "prefix/plots/calinski-harabasz-avg-silhouette.png", width = "774px", height = "598px")),
      tags$h4("This silhouette plot is a little bit different than the previous one, because it evaluates the optimal number
              of clusters using the Calinski-Harabasz (CH) criterion. The CH index focuses on maximising variance between clusters
              and minimises variance within clusters - ergo it technically forces compact and well-separated clusters. The silhouette
              values shown here evaluate the clustering's internal consistency, independently verifying how well each document (script)
              fits within its assigned group.", br(), br(),
              "In this case, the plot is mainly for showcasing how it shouldn't look like, because there are only 10
              documents, so the algorithm doesn't have much to choose from and the less documents, the smaller chance to have
              coherent and well-aligned clusters.",
              style = 'text-align: left; font-style: italic;')
  ),
  
  tags$hr(style = "border-top: 5px solid #B52400;"),
)
