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
package org.apache.myfaces.extensions.validator.beanval.metadata.transformer;

import org.apache.myfaces.extensions.validator.core.metadata.CommonMetaDataKeys;

import javax.validation.constraints.Size;
import javax.validation.metadata.ConstraintDescriptor;
import java.util.Map;
import java.util.HashMap;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
public class StringSizeMetaDataTransformer extends AbstractBeanValidationMetaDataTransformer<Size>
{
    protected Map<String, Object> convertConstraintDescriptor(ConstraintDescriptor<Size> constraintDescriptor)
    {
        Map<String, Object> results = new HashMap<String, Object>();
        Size annotation = constraintDescriptor.getAnnotation();

        int minimum = annotation.min();

        if(minimum != 0)
        {
            results.put(CommonMetaDataKeys.MIN_LENGTH, minimum);
            results.put(CommonMetaDataKeys.WEAK_REQUIRED, true);
        }
        else
        {
            results.put(CommonMetaDataKeys.MIN_LENGTH_DEFAULT, minimum);
        }

        int maximum = annotation.max();
        if(maximum != Integer.MAX_VALUE)
        {
            results.put(CommonMetaDataKeys.MAX_LENGTH, maximum);
        }
        else
        {
            results.put(CommonMetaDataKeys.MAX_LENGTH_DEFAULT, maximum);
        }

        return results;
    }
}
