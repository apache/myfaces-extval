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
package org.apache.myfaces.extensions.validator.core.metadata;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

/**
 * helper for frequent meta-data keys
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
/*
 * placed in core to avoid duplicated information in multiple modules (validation module and component-support module)
 */
@UsageInformation(UsageCategory.INTERNAL)
public interface CommonMetaDataKeys
{
    static final String REQUIRED = "required";
    static final String WEAK_REQUIRED = "weak_required";

    static final String MIN_LENGTH = "min_length";
    static final String MIN_LENGTH_DEFAULT = "min_length_default";

    static final String MAX_LENGTH = "max_length";
    static final String MAX_LENGTH_DEFAULT = "max_length_default";

    static final String RANGE_MIN = "range_min";
    static final String RANGE_MIN_DEFAULT = "range_min_default";

    static final String RANGE_MAX = "range_max";
    static final String RANGE_MAX_DEFAULT = "range_max_default";

    static final String PATTERN = "pattern";
    static final String PATTERN_VALIDATION_ERROR_MESSAGE = "pattern_validation_error_message";
    static final String EMAIL = "email";

    static final String CUSTOM = "custom";
    static final String SKIP_VALIDATION = "skip_validation";
    static final String DISABLE_CLIENT_SIDE_VALIDATION = "disable_client_side_validation";
    //available for add-ons not used internally due to performance reasons
    static final String DISABLE_SHOW_INDICATION = "disable_show_indication";
}
