<template>
  <div class="reset">
    <b-form class="form-reset text-center" @submit.prevent="onSubmit">
      <br />
      <h3 class="mb-8">Reset your password</h3>
      <br />
      <b-form-input
        id="email-address"
        type="email"
        v-model="emailAddress"
        required
        placeholder="Email address"
      ></b-form-input>

      <b-form-invalid-feedback :state="isValidEmail">
        Invalid email address.
      </b-form-invalid-feedback>

      <b-button
        :disabled="loading"
        variant="primary"
        block
        size="lg"
        type="submit"
        class="mt-4"
      >
        Get Reset Code
      </b-button>
      <br />
    </b-form>

    <b-modal
      id="reset-modal"
      ref="modal"
      title="Use code to reset your password"
      @ok="resetPass"
    >
      <b-form-input
        id="reset-code"
        type="password"
        v-model="resetCode"
        required
        placeholder="Reset code emailed from Voltron.sys"
      ></b-form-input>

      <b-form-input
        id="new-password"
        type="password"
        v-model="resetPassword"
        required
        placeholder="New password"
      ></b-form-input>

      <b-form-invalid-feedback :state="isValidReset">
        Invalid reset code
      </b-form-invalid-feedback>
    </b-modal>

    <b-modal
      id="reset-success-modal"
      ref="modal"
      title="Reset Successful!"
      @ok="repeatLogin"
    >
      Please login with your new password.
    </b-modal>

    <b-modal id="reset-failure-modal" ref="modal" title="Reset Failed!">
      Please try again!
    </b-modal>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import { ResetInfo } from "@/types";
import ApiService from "@/services/api";

@Component
export default class Reset extends Vue {
  emailAddress = "";
  resetPassword = "";
  resetCode = "";
  loading = false;

  onSubmit() {
    this.loading = true;
    const reset: ResetInfo = {
      emailAddress: this.emailAddress
    };
    ApiService.reset(reset)
      .then(() => {
        this.$bvModal.show("reset-modal");
      })
      .catch(() => {
        const msg = "There is no account associated with that address!";
        this.showMessage(msg, "Sorry!", "danger");
      });
  }

  showMessage(msg: string, title: string, variant: string) {
    this.$bvToast.toast(msg, {
      toaster: "b-toaster-top-center",
      solid: true,
      title,
      variant
    });
  }

  resetPass() {
    const resetInfo = {
      email: this.emailAddress,
      password: this.resetPassword,
      code: this.resetCode
    };
    ApiService.resetPass(resetInfo)
      .then(() => {
        this.$bvModal.show("reset-success-modal");
      })
      .catch(() => {
        this.$bvModal.show("reset-failure-modal");
      });
  }
}
</script>

<style lang="scss">
.reset {
  width: 100%;
  display: flex;
  align-items: center;
}

.form-reset {
  width: 100%;
  max-width: 350px;
  padding: 15px;
  margin: 0 auto;
}

.form-reset input[type="email"] {
  margin-bottom: -1px;
  border-bottom-right-radius: 0;
  border-bottom-left-radius: 0;
}

.form-reset input[type="password"] {
  margin-bottom: 10px;
  border-top-left-radius: 0;
  border-top-right-radius: 0;
}
.form-reset .form-control {
  height: auto;
  position: relative;
  box-sizing: border-box;
  padding: 10px;
  font-size: 16px;
}
</style>
