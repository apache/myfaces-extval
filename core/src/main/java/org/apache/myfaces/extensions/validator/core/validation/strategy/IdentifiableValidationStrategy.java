/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.core.validation.strategy;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

/**
 * if an adapter (ValidationStrategy only used for component initialization) is used for several constraints, this
 * interface allows to detect the type of current constraint.
 * It's needed for constraint mappings which don't follow the original approach of ExtVal (e.g. bv)
 * 
 * @since x.x.3
 */
@UsageInformation(UsageCategory.API)
public interface IdentifiableValidationStrategy extends ValidationStrategy
{
    /**
     * Separator which should be used by a meta-data transformer factory to create an unambiguous key that consists of
     * the name of the {@link org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy}
     * and the id provided by {@link #getId()} which allows to select the correct
     * {@link org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer} implementation
     * if an adapter {@link org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy} is
     * responsible for multiple constraints.
     */
    String ID_PREFIX = ":";

    /**
     * Returns the unique part for the key which is used to select the correct
     * {@link org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer} if the
     * {@link org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy} is
     * responsible for multiple constraints.
     * @return Unique String to map the current instance of an adapter
     * {@link org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy} .
     */
    String getId();
}