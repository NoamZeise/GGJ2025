LISP := "sbcl"
exec-name := ggj2025
build-dir := bin

code := $(wildcard src/**.lisp)
gficl-code := $(wildcard gficl/src/**.lisp)
build-deps := $(code) ggj2025.asd $(gficl-code) gficl/gficl.asd

assets-src := $(wildcard assets/*)
assets := $(assets-src:%=$(build-dir)/%)

shaders-src := $(wildcard shaders/*)
shaders := $(shaders-src:%=$(build-dir)/%)

.PHONY: build
build: build-setup $(build-dir)/$(exec-name)

$(build-dir)/$(exec-name): $(build-deps)
	$(LISP)	--eval "(ql:quickload :deploy)" \
		--load ggj2025.asd \
		--eval "(ql:quickload :ggj2025)" \
		--eval "(asdf:make :ggj2025)"

# build without quicklisp
.PHONY: asdf
asdf: build-setup $(build-deps)
	$(LISP)	--load project.asd \
                --eval "(asdf:load-system :ggj2025)" \
                --eval "(asdf:make :ggj2025)"

.PHONY: build-setup
build-setup: gficl $(assets) $(shaders)

gficl:
	git clone https://github.com/NoamZeise/gficl.git

$(build-dir)/assets/%: assets/% | $(build-dir)/assets
	cp -r $< $@

$(build-dir)/shaders/%: shaders/% | $(build-dir)/shaders
	cp -r $< $@

$(build-dir)/assets: | $(build-dir)
	mkdir -p $(build-dir)/assets
$(build-dir)/shaders: | $(build-dir)
	mkdir -p $(build-dir)/shaders
$(build-dir):
	mkdir -p $(build-dir)

.PHONY: repl
repl: gficl
	$(LISP) --load repl-setup.lisp \
	     	--eval "(ggj2025:run)"

.PHONY: clean
clean:
	rm -rf $(build-dir)

.PHONY: cleanall
cleanall: clean
	rm -rf gficl/

.PHONY: pull
pull: gficl
	git pull
	cd gficl
	git pull
