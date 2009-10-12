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
package org.apache.myfaces.extensions.validator.core.metadata.transformer.mapper;

import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.core.mapper.SubMapperAwareNameMapper;
import org.apache.myfaces.extensions.validator.core.mapper.SubNameMapper;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import java.util.List;
import java.util.ArrayList;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation({UsageCategory.INTERNAL})
public class ValidationStrategyToMetaDataTransformerSubMapperAwareNameMapper
    extends AbstractValidationStrategyToMetaDataTransformerNameMapper
    implements SubMapperAwareNameMapper<ValidationStrategy>
{
    private List<NameMapper<ValidationStrategy>> subNameMappers = new ArrayList<NameMapper<ValidationStrategy>>();

    public void addNameMapper(SubNameMapper<ValidationStrategy> nameMapper)
    {
        if(!this.subNameMappers.contains(nameMapper))
        {
            this.subNameMappers.add(nameMapper);
        }
    }

    public String createName(ValidationStrategy source)
    {
        String result = null;

        for(NameMapper<ValidationStrategy> mapper : this.subNameMappers)
        {
            result = mapper.createName(source);

            if(result != null)
            {
                return result;
            }
        }
        return result;
    }
}