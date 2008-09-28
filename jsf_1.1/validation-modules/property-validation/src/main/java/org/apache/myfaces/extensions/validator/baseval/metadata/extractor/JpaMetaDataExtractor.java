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
package org.apache.myfaces.extensions.validator.baseval.metadata.extractor;

import org.apache.myfaces.extensions.validator.core.metadata.CommonMetaDataKeys;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;

import javax.persistence.Basic;
import javax.persistence.Column;
import java.lang.annotation.Annotation;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
public class JpaMetaDataExtractor implements MetaDataExtractor
{
    @ToDo(value = Priority.HIGH, description = "impl. the rest")
    public Map<String, Object> extractMetaData(Annotation annotation)
    {
        Map<String, Object> results = new HashMap<String, Object>();

        if(annotation instanceof Column)
        {
            if(!((Column) annotation).nullable())
            {
                results.put(CommonMetaDataKeys.REQUIRED, true);
            }

            results.put(CommonMetaDataKeys.MAX_LENGTH, ((Column) annotation).length());
        }
        else if(annotation instanceof Basic)
        {
            if(!((Basic)annotation).optional())
            {
                results.put(CommonMetaDataKeys.REQUIRED, true);
            }
        }
        //TODO impl. the rest!!!
        return results;
    }
}
