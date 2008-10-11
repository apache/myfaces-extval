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

import org.apache.myfaces.extensions.validator.core.metadata.CommonMetaDataKeys;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.AbstractMetaDataTransformer;
import org.apache.myfaces.extensions.validator.core.annotation.AnnotationEntry;
import org.apache.myfaces.extensions.validator.baseval.annotation.Required;

import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
public class RequiredMetaDataTransformer extends AbstractMetaDataTransformer
{
    protected Map<String, Object> convert(AnnotationEntry annotationEntry)
    {
        Map<String, Object> results = new HashMap<String, Object>();
        results.put(CommonMetaDataKeys.WEAK_REQUIRED, true);
        return results;
    }

    @Override
    protected String getSkipExpression(AnnotationEntry annotationEntry)
    {
        return ((Required)(annotationEntry).getAnnotation()).skipValidation();
    }

    //returns the key of the skiped meta-data e.g.
    @Override
    protected List<String> getMetaDataKeys()
    {
        List<String> skippedMetaDataList = new ArrayList<String>();
        skippedMetaDataList.add(CommonMetaDataKeys.WEAK_REQUIRED);
        return skippedMetaDataList;
    }
}