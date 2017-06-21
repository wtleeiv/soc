# SOC - Stream of Consciousness
a flexible html writer
## How to Install
* best used with [lww](https://github.com/wtleeiv/lww)
* clone this repo and soc repo into a location within ASDF source registry (eg. `~/.roswell/local-projects/`)
  * if you have no idea what the ASDF source registry is, check [this](https://common-lisp.net/project/asdf/asdf/Configuring-ASDF-to-find-your-systems.html) out 
* now that both projects are found by quicklisp you are ready to start writing web apps in lisp!!
## How to Use
Example `body_html.lisp`
~~~~
(soc:soc
  (:body
   (:div :class container
         (:header
          (:h1 "Todo Lisp")

          (:label :class "hide-completed"
                  (:input :type "checkbox" "Hide completed tasks"))

          (:form :class "new-task"
                 (:input :type text :name text :placeholder "Type to add a task")))

         (:ul
          ("{{#each tasks}}")
          ("{{> task}}")
          ("{{/each}}")))))
~~~~
Produces `body.html`
~~~~
<body>
	<div class="container">
		<header>
			<h1>Todo Lisp
			</h1>
			<label class="hide-completed">
				<input type="checkbox">Hide completed tasks
			</label>
			<form class="new-task">
				<input type="text" name="text" placeholder="Type to add a task">
			</form>
		</header>
		<ul>
			{{#each tasks}}
			{{> task}}
			{{/each}}
		</ul>
	</div>
</body>
~~~~
