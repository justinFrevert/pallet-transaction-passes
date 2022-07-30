#![cfg_attr(not(feature = "std"), no_std)]

use frame_support::traits::IsSubType;
pub use pallet::*;
use scale_info::TypeInfo;
use sp_runtime::traits::SignedExtension;
use codec::{Decode, Encode};
use sp_std::marker::PhantomData;
use sp_runtime::{
	traits::{DispatchInfoOf, PostDispatchInfoOf},
	transaction_validity::{
		InvalidTransaction, TransactionValidity, TransactionValidityError, ValidTransaction,
	},
};

#[cfg(test)]
mod mock;

#[cfg(test)]
mod tests;

#[cfg(feature = "runtime-benchmarks")]
mod benchmarking;

#[frame_support::pallet]
pub mod pallet {
	use frame_support::pallet_prelude::*;
	use frame_system::pallet_prelude::*;

	#[pallet::config]
	pub trait Config: frame_system::Config {
		#[pallet::constant]
		type Day: Get<u32>;
		#[pallet::constant]
		type Hour: Get<u32>;
	}

	#[pallet::pallet]
	#[pallet::generate_store(pub(super) trait Store)]
	pub struct Pallet<T>(_);

	#[pallet::storage]
	#[pallet::getter(fn get_timed_pass)]
	pub(super) type TimedPass<T: Config> = StorageMap<
		_,
		Blake2_128Concat,
		T::AccountId,
		PassInfo<T::BlockNumber>,
		OptionQuery
	>;

	// There's not much usefulness to the idea if there's only one type of pass
	#[derive(Debug, Encode, Decode, Clone, Eq, PartialEq, TypeInfo, MaxEncodedLen)]
	pub enum PassType {
		Hourly,
		// Usage(u8),   
		// Day
		// TotalWeight,
	}

	#[derive(Encode, Decode, TypeInfo, MaxEncodedLen)]
	// Info about pass. In the end, it would be nicer if it wraps NFTs or is involved with NFT metadata somehow
	pub struct PassInfo<BlockNumber> {
		pub pass_type: PassType,
		pub expiration: BlockNumber
	}

	impl<BlockNumber> PassInfo<BlockNumber> {
		fn new(pass_type: PassType, expiration: BlockNumber) -> Self {
			PassInfo { pass_type, expiration }
		}
	}

	// Give different extrinsics per type to simplify benchmarking and weights, since there should be more thought on weight here
	#[pallet::call]
	impl<T: Config> Pallet<T> {
		// TODO: Think about how to handle weights with something like this beyond benchmarking
		#[pallet::weight(10_000 + T::DbWeight::get().writes(1))]
		pub fn create_chain_hourly_pass(
			origin: OriginFor<T>,
			owner: T::AccountId
		) -> DispatchResult {
			ensure_signed(origin)?;

			let current_block: T::BlockNumber = <frame_system::Pallet<T>>::block_number().into();
			let expiration = current_block + T::Hour::get().into();
			TimedPass::<T>::insert(owner, PassInfo::new(PassType::Hourly, expiration));
			Ok(())
		}

		// Similar extrinsics...
		#[pallet::weight(10_000 + T::DbWeight::get().writes(1))]
		pub fn create_chain_daily_pass(
			origin: OriginFor<T>,
			_owner: T::AccountId
		) -> DispatchResult {
			ensure_signed(origin)?;
			Ok(())
		}
	}
}

#[derive(Encode, Decode, Clone, Eq, PartialEq, TypeInfo)]

pub struct CheckTxPass<T: Config + Send + Sync>(PhantomData<T>);

impl<T: Config + Send + Sync> CheckTxPass<T> {
	pub fn new() -> Self {
		CheckTxPass(PhantomData)
	}
}

impl<T: Config + Send + Sync> sp_std::fmt::Debug for CheckTxPass<T> {
	fn fmt(&self, f: &mut sp_std::fmt::Formatter) -> sp_std::fmt::Result {
		write!(f, "CheckTxPass")
	}
}

impl<T: Config + Send + Sync + TypeInfo> SignedExtension for CheckTxPass<T>
where
	T::Call: IsSubType<Call<T>>
{
	const IDENTIFIER: &'static str = "CheckTxPass";
	type AccountId = T::AccountId;
	type Call = T::Call;
	type AdditionalSigned = ();
	type Pre = ();

	fn additional_signed(&self) -> sp_std::result::Result<(), TransactionValidityError> {
		Ok(())
	}

	fn validate(
		&self,
		who: &Self::AccountId,
		call: &Self::Call,
		_info: &DispatchInfoOf<Self::Call>,
		_len: usize,
	) -> TransactionValidity {

		// Temporary only. To allow for testing. A more mature setup would involve some assignment of passes at genesis
		match call.is_sub_type() {
			Some(Call::create_chain_hourly_pass { .. }) => Ok(ValidTransaction::default()),
			_ => {
				match Pallet::<T>::get_timed_pass(who) {
					Some(pass) if <frame_system::Pallet<T>>::block_number() > pass.expiration  => {
						Err(TransactionValidityError::Invalid(InvalidTransaction::Payment))
					},
					None => Err(TransactionValidityError::Invalid(InvalidTransaction::Payment)),
					_ => Ok(ValidTransaction {
						..Default::default()
					})
				}
			}
		}
		// TODO: some check against max weight used per tx, per pass
	}

	fn pre_dispatch(
		self,
		who: &Self::AccountId,
		call: &Self::Call,
		_info: &DispatchInfoOf<Self::Call>,
		_len: usize,
	) -> Result<Self::Pre, TransactionValidityError> {
		match call.is_sub_type() {
			Some(Call::create_chain_hourly_pass { .. }) => Ok(()),
			_ => {
				match Pallet::<T>::get_timed_pass(who) {
					Some(pass) if <frame_system::Pallet<T>>::block_number() > pass.expiration  => {
						Err(TransactionValidityError::Invalid(InvalidTransaction::Payment))
					},
					None => Err(TransactionValidityError::Invalid(InvalidTransaction::Payment)),
					_ => Ok(())
				}
			}
		}
		// TODO: some check against max weight used per tx, per pass
	}

	fn post_dispatch(
		_pre: Option<Self::Pre>,
		_info: &DispatchInfoOf<Self::Call>,
		_post_info: &PostDispatchInfoOf<Self::Call>,
		_len: usize,
		_result: &sp_runtime::DispatchResult,
	) -> Result<(), TransactionValidityError> {
		// TODO check weight used and add it to some storage tracking some storage used by the account against some max threshold per pass
		Ok(())
	}

}