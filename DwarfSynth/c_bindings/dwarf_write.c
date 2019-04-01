/** dwarf_write -- set of functions to add a given DWARF table to an ELF file
 *
 * Mostly based on work by emersion on Dareog
 * <https://github.com/emersion/dareog>
 */

#include "dwarf_write.h"


struct internal_state {
    Elf *elf;
};

static Elf_Scn *find_section_by_name(Elf *elf, const char *section_name) {
	size_t sections_num;
	if (elf_getshdrnum(elf, &sections_num)) {
		return NULL;
	}

	size_t shstrndx;
	if (elf_getshdrstrndx(elf, &shstrndx)) {
		return NULL;
	}

	for (size_t i = 0; i < sections_num; ++i) {
		Elf_Scn *s = elf_getscn(elf, i);
		if (s == NULL) {
			return NULL;
		}

		GElf_Shdr sh;
		if (!gelf_getshdr(s, &sh)) {
			return NULL;
		}

		char *name = elf_strptr(elf, shstrndx, sh.sh_name);
		if (name == NULL) {
			return NULL;
		}

		if (strcmp(name, section_name) == 0) {
			return s;
		}
	}

	return NULL;
}

static Elf_Scn *create_section(Elf *elf, const char *name) {
	Elf_Scn *scn = elf_newscn(elf);
	if (scn == NULL) {
		fprintf(stderr, "elf_newscn() failed: %s\n", elf_errmsg(-1));
		return NULL;
	}

	GElf_Shdr shdr;
	if (!gelf_getshdr(scn, &shdr)) {
		fprintf(stderr, "gelf_getshdr() failed\n");
		return NULL;
	}

	// Add section name to .shstrtab
	Elf_Scn *shstrtab = find_section_by_name(elf, ".shstrtab");
	if (shstrtab == NULL) {
		fprintf(stderr, "can't find .shstrtab section\n");
		return NULL;
	}

	GElf_Shdr shstrtab_shdr;
	if (!gelf_getshdr(shstrtab, &shstrtab_shdr)) {
		fprintf(stderr, "gelf_getshdr(shstrtab) failed\n");
		return NULL;
	}

	Elf_Data *shstrtab_data = elf_newdata(shstrtab);
	if (shstrtab_data == NULL) {
		fprintf(stderr, "elf_newdata(shstrtab) failed\n");
		return NULL;
	}
	shstrtab_data->d_buf = strdup(name);
	shstrtab_data->d_size = strlen(name) + 1;
	shstrtab_data->d_align = 1;

	shdr.sh_name = shstrtab_shdr.sh_size;
	shstrtab_shdr.sh_size += shstrtab_data->d_size;

	if (!gelf_update_shdr(scn, &shdr)) {
		fprintf(stderr, "gelf_update_shdr() failed\n");
		return NULL;
	}

	if (!gelf_update_shdr(shstrtab, &shstrtab_shdr)) {
		fprintf(stderr, "gelf_update_shdr(shstrtab) failed\n");
		return NULL;
	}

	return scn;
}

static int find_section_symbol(Elf *elf, size_t index, GElf_Sym *sym) {
	Elf_Scn *symtab = find_section_by_name(elf, ".symtab");
	if (symtab == NULL) {
		fprintf(stderr, "can't find .symtab section\n");
		return -1;
	}

	Elf_Data *symtab_data = elf_getdata(symtab, NULL);
	if (symtab_data == NULL) {
		fprintf(stderr, "elf_getdata(symtab) failed\n");
		return -1;
	}

	GElf_Shdr symtab_shdr;
	if (!gelf_getshdr(symtab, &symtab_shdr)) {
		fprintf(stderr, "gelf_getshdr(symtab) failed\n");
		return -1;
	}

	int symbols_nr = symtab_shdr.sh_size / symtab_shdr.sh_entsize;
	for (int i = 0; i < symbols_nr; ++i) {
		if (!gelf_getsym(symtab_data, i, sym)) {
			fprintf(stderr, "gelf_getsym() failed\n");
			continue;
		}

		if (GELF_ST_TYPE(sym->st_info) == STT_SECTION &&
				index == sym->st_shndx) {
			return i;
		}
	}

	return -1;
}

static Elf_Scn *create_debug_frame_section(Elf *elf, const char *name,
		char *buf, size_t len) {
	Elf_Scn *scn = create_section(elf, name);
	if (scn == NULL) {
		return NULL;
	}

	Elf_Data *data = elf_newdata(scn);
	if (data == NULL) {
		fprintf(stderr, "elf_newdata() failed: %s\n", elf_errmsg(-1));
		return NULL;
	}
	data->d_align = 4;
	data->d_buf = buf;
	data->d_size = len;

	GElf_Shdr shdr;
	if (!gelf_getshdr(scn, &shdr)) {
		fprintf(stderr, "gelf_getshdr() failed\n");
		return NULL;
	}
	shdr.sh_size = len;
	shdr.sh_type = SHT_PROGBITS;
	shdr.sh_addralign = 1;
	shdr.sh_flags = SHF_ALLOC;
	if (!gelf_update_shdr(scn, &shdr)) {
		fprintf(stderr, "gelf_update_shdr() failed\n");
		return NULL;
	}

	return scn;
}

static Elf_Scn *create_rela_section(Elf *elf, const char *name, Elf_Scn *base,
		char *buf, size_t len) {
	Elf_Scn *scn = create_section(elf, name);
	if (scn == NULL) {
		fprintf(stderr, "can't create rela section\n");
		return NULL;
	}

	Elf_Data *data = elf_newdata(scn);
	if (!data) {
		fprintf(stderr, "elf_newdata() failed\n");
		return NULL;
	}

	data->d_buf = buf;
	data->d_size = len;
	data->d_align = 1;

	Elf_Scn *symtab = find_section_by_name(elf, ".symtab");
	if (symtab == NULL) {
		fprintf(stderr, "can't find .symtab section\n");
		return NULL;
	}

	GElf_Shdr shdr;
	if (!gelf_getshdr(scn, &shdr)) {
		fprintf(stderr, "gelf_getshdr() failed\n");
		return NULL;
	}
	shdr.sh_size = data->d_size;
	shdr.sh_type = SHT_RELA;
	shdr.sh_addralign = 8;
	shdr.sh_link = elf_ndxscn(symtab);
	shdr.sh_info = elf_ndxscn(base);
	shdr.sh_flags = SHF_INFO_LINK;
	if (!gelf_update_shdr(scn, &shdr)) {
		fprintf(stderr, "gelf_update_shdr() failed\n");
		return NULL;
	}

	return scn;
}

static int write_fde_instruction(struct dwarfw_fde *fde,
		struct pre_dwarf_entry *cur_entry, FILE *f) {
	reg_t reg = cur_entry->cfa_offset_reg;
	if (reg == DW_REG_INV) {
		fprintf(stderr, "warning: dwarf_write: undefined CFA at "
                "0x%" PRIxPTR "\n",
                cur_entry->location);

		// write an undefined ra
		dwarfw_cie_write_undefined(fde->cie, 16, f);
	} else if (reg <= DW_MAX_REG) {
		dwarfw_cie_write_def_cfa(fde->cie, reg, cur_entry->cfa_offset, f);

		// ra's offset is fixed at -8
		dwarfw_cie_write_offset(fde->cie, 16, -8, f);
	} else {
		fprintf(stderr,
                "error: dwarf_write: unsupported register %d at 0x%" PRIxPTR
                " as CFA offset\n",
                reg, cur_entry->location);
		return -1;
	}

  if(cur_entry->rbp_defined)
    dwarfw_cie_write_offset(fde->cie, 6, cur_entry->rbp_offset, f);
  else
    dwarfw_cie_write_undefined(fde->cie, 6, f);

	return 0;
}

static int write_all_fde_instructions(struct dwarfw_fde *fde,
        struct pre_dwarf_fde *cur_source, FILE *f)
{
	for (size_t entry_id = 0; entry_id < cur_source->num; entry_id++) {
        int rv = write_fde_instruction(
                fde, &(cur_source->entries[entry_id]), f);
        if(rv != 0) {
				return -1;
        }
        if(entry_id != cur_source->num - 1) { // Not the last row
            addr_t loc_delta =
                cur_source->entries[entry_id + 1].location
                - cur_source->entries[entry_id].location;

            dwarfw_cie_write_advance_loc(fde->cie, loc_delta, f);
        }
	}

	return 0;
}

static int process_section(struct internal_state* state,
        struct pre_dwarf* pre_dwarf, Elf_Scn *s, FILE *f,
        size_t *written /*, FILE *rela_f */)
{
	size_t shndx = elf_ndxscn(s);

	GElf_Sym text_sym;
	int text_sym_idx = find_section_symbol(state->elf, shndx, &text_sym);
	if (text_sym_idx < 0) {
		fprintf(stderr, "can't find .text section in symbol table\n");
		return 1;
	}

    GElf_Shdr shdr;
    gelf_getshdr(s, &shdr);
    uintptr_t s_addr = shdr.sh_addr,
              s_endaddr = shdr.sh_addr + shdr.sh_size;


	struct dwarfw_cie cie = {
		.version = 1,
		.augmentation = "zR",
		.code_alignment = 1,
		.data_alignment = -8,
		.return_address_register = 16,
		.augmentation_data = {
			.pointer_encoding = DW_EH_PE_sdata4 /* | DW_EH_PE_pcrel */,
		},
	};

	size_t n;
	if (!(n = dwarfw_cie_write(&cie, f))) {
		fprintf(stderr, "dwarfw_cie_write() failed\n");
		return -1;
	}
	*written += n;

	// Generate the FDEs
	for (size_t fde_id = 0; fde_id < pre_dwarf->num_fde; ++fde_id) {
        struct pre_dwarf_fde* cur_fde = &(pre_dwarf->fdes[fde_id]);

        if(!(s_addr <= cur_fde->initial_location
                    && cur_fde->end_location < s_endaddr))
            // The fde is not included in this section
        {
            continue;
        }
		struct dwarfw_fde fde = {
			.cie = &cie,
			.initial_location = cur_fde->initial_location,
		};

		char *instr_buf;
		size_t instr_len;
		FILE *instr_f = open_memstream(&instr_buf, &instr_len);
		if (instr_f == NULL) {
			fprintf(stderr, "open_memstream() failed\n");
			return -1;
		}

		if (write_all_fde_instructions(&fde, cur_fde, instr_f)) {
			fprintf(stderr, "write_all_fde_instructions() failed\n");
			return -1;
		}
		fclose(instr_f);

		if (instr_len == 0) {
			continue;
		}

		fde.address_range = cur_fde->end_location - cur_fde->initial_location;
		fde.instructions_length = instr_len;
		fde.instructions = instr_buf;
		fde.cie_pointer = *written;

		/* GElf_Rela initial_position_rela; */
		if (!(n = dwarfw_fde_write(&fde, /*&initial_position_rela*/NULL, f))) {
			fprintf(stderr, "dwarfw_fde_write() failed\n");
			return -1;
		}
		/* initial_position_rela.r_offset += *written; */
		*written += n;
		free(instr_buf);

		// r_offset and r_addend have already been populated by dwarfw_fde_write
        /*
		initial_position_rela.r_info = GELF_R_INFO(text_sym_idx,
			ELF32_R_TYPE(initial_position_rela.r_info));

		if (!fwrite(&initial_position_rela, 1, sizeof(GElf_Rela), rela_f)) {
			fprintf(stderr, "can't write rela\n");
			return 1;
		}
        */
	}

	return 0;
}

int write_dwarf(char* objname, char* eh_path, struct pre_dwarf* pre_dwarf) {
	elf_version(EV_CURRENT);

    FILE* out_dwarf = fopen(eh_path, "a"); // Create file
    fclose(out_dwarf);
    out_dwarf = fopen(eh_path, "w"); // Truncate and open for writing

	int fd = open(objname, O_RDWR);
	if (fd == -1) {
		fprintf(stderr, "cannot open file\n");
		return -1;
	}

	Elf *elf = elf_begin(fd, ELF_C_RDWR_MMAP, NULL);
	if (!elf) {
		fprintf(stderr, "elf_begin() failed\n");
		return -1;
	}

	// Check the ELF object
	Elf_Kind ek = elf_kind(elf);
	if (ek != ELF_K_ELF) {
		fprintf(stderr, "not an ELF object\n");
		return 1;
	}

	size_t sections_num;
	if (elf_getshdrnum(elf, &sections_num)) {
		return 1;
	}

	size_t shstrtab_idx;
	if (elf_getshdrstrndx(elf, &shstrtab_idx)) {
		fprintf(stderr, "elf_getshdrstrndx() failed\n");
		return -1;
	}

	struct internal_state state = { .elf = elf };

    /*
	char *buf;
	size_t len;
	FILE *f = open_memstream(&buf, &len);
	if (f == NULL) {
		fprintf(stderr, "open_memstream() failed\n");
		return 1;
	}
    */

    /*
	char *rela_buf;
	size_t rela_len;
	FILE *rela_f = open_memstream(&rela_buf, &rela_len);
	if (rela_f == NULL) {
		fprintf(stderr, "open_memstream() failed\n");
		return 1;
	}
    */

	size_t written = 0;
	for (size_t i = 0; i < sections_num; ++i) {
		Elf_Scn *s = elf_getscn(elf, i);
		if (s == NULL) {
			return 1;
		}

		GElf_Shdr sh;
		if (!gelf_getshdr(s, &sh)) {
			return 1;
		}

		if ((sh.sh_flags & SHF_EXECINSTR) == 0) {
			continue;
		}

		if (process_section(&state, pre_dwarf, s, out_dwarf, &written /*, rela_f*/)) {
			return 1;
		}
	}
	// fclose(f);
	/* fclose(rela_f); */

	// Create the .eh_frame section
    /*
	Elf_Scn *scn = create_debug_frame_section(elf, ".eh_frame", buf, len);
	if (scn == NULL) {
		fprintf(stderr, "create_debug_frame_section() failed\n");
		return 1;
	}
    */

    /*
	// Create the .eh_frame.rela section
	Elf_Scn *rela = create_rela_section(elf, ".rela.eh_frame", scn,
		rela_buf, rela_len);
	if (rela == NULL) {
		fprintf(stderr, "create_rela_section() failed\n");
		return 1;
	}
    */

	// Write the modified ELF object
    /*
	elf_flagelf(elf, ELF_C_SET, ELF_F_DIRTY);
	if (elf_update(elf, ELF_C_WRITE) < 0) {
		fprintf(stderr, "elf_update() failed: %s\n", elf_errmsg(-1));
		return 1;
	}
    */

	// free(buf);
	/* free(rela_buf); */

	elf_end(elf);
	close(fd);
    fclose(out_dwarf);

	return 0;
}
