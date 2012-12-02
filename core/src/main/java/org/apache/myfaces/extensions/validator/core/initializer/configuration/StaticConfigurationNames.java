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
package org.apache.myfaces.extensions.validator.core.initializer.configuration;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

/**
 * List of all artifacts which can be configured via static mappings ({@link StaticConfiguration}).
 *
 * @since 1.x.1
 */
@UsageInformation({UsageCategory.API})
public enum StaticConfigurationNames
{
    META_DATA_TO_VALIDATION_STRATEGY_CONFIG,
    VALIDATION_STRATEGY_TO_MESSAGE_RESOLVER_CONFIG,
    VALIDATION_STRATEGY_TO_META_DATA_TRANSFORMER_CONFIG,
    STORAGE_TYPE_TO_STORAGE_MANAGER_CONFIG,

    SKIP_VALIDATION_SUPPORT_CONFIG,
    VALIDATION_PARAMETER_CONFIG
}
