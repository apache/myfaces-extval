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
package org.apache.myfaces.extensions.validator.crossval.storage;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultCrossValidationStorage implements CrossValidationStorage
{
    private List<CrossValidationStorageEntry> crossValidationStorageEntries =
        new ArrayList<CrossValidationStorageEntry>();

    public void add(CrossValidationStorageEntry entry)
    {
        this.crossValidationStorageEntries.add(entry);
    }

    public List<CrossValidationStorageEntry> getCrossValidationStorageEntries()
    {
        return crossValidationStorageEntries;
    }

    public void setCrossValidationStorageEntries(List<CrossValidationStorageEntry> crossValidationStorageEntries)
    {
        this.crossValidationStorageEntries = crossValidationStorageEntries;
    }
}
