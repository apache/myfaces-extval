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
package org.apache.myfaces.extensions.validator.baseval.metadata.transformer;

import org.apache.myfaces.extensions.validator.baseval.annotation.LongRange;
import org.apache.myfaces.extensions.validator.core.metadata.CommonMetaDataKeys;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class LongRangeMetaDataTransformer extends AbstractValidationParameterAwareTransformer
{
    protected Map<String, Object> transformMetaData(MetaDataEntry metaDataEntry)
    {
        Map<String, Object> results = new HashMap<String, Object>();
        LongRange annotation = metaDataEntry.getValue(LongRange.class);

        long minimum = annotation.minimum();

        if(minimum != Long.MIN_VALUE)
        {
            results.put(CommonMetaDataKeys.RANGE_MIN, minimum);
        }
        else
        {
            results.put(CommonMetaDataKeys.RANGE_MIN_DEFAULT, minimum);
        }

        long maximum = annotation.maximum();

        if(maximum != Long.MAX_VALUE)
        {
            results.put(CommonMetaDataKeys.RANGE_MAX, maximum);
        }
        else
        {
            results.put(CommonMetaDataKeys.RANGE_MAX_DEFAULT, maximum);
        }

        return results;
    }
}
