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

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.beanval.payload.DisableClientSideValidation;

import javax.validation.metadata.ConstraintDescriptor;
import javax.validation.Payload;
import java.util.Map;
import java.util.HashMap;
import java.lang.annotation.Annotation;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation({UsageCategory.REUSE})
public abstract class AbstractBeanValidationMetaDataTransformer<T extends Annotation> implements MetaDataTransformer
{
    public Map<String, Object> convertMetaData(MetaDataEntry metaDataEntry)
    {
        ConstraintDescriptor<? extends T> constraintDescriptor = metaDataEntry.getValue(ConstraintDescriptor.class);

        if(isClientSideValidationEnabled(constraintDescriptor))
        {
            return convertConstraintDescriptor((ConstraintDescriptor<T>)constraintDescriptor);
        }
        return new HashMap<String, Object>();
    }

    private boolean isClientSideValidationEnabled(ConstraintDescriptor<? extends T> constraintDescriptor)
    {
        for(Class<? extends Payload> payload : constraintDescriptor.getPayload())
        {
            if(DisableClientSideValidation.class.isAssignableFrom(payload))
            {
                return false;
            }
        }
        return true;
    }

    protected abstract Map<String, Object> convertConstraintDescriptor(ConstraintDescriptor<T> constraintDescriptor);
}
